use proc_macro2::*;
use quote::*;
use std::sync::atomic::*;
use syn::*;
use syn::parse::Parser;
use syn::punctuated::*;

#[proc_macro]
pub fn crate_version(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    assert!(input.is_empty(), "Unrecognized arguments");
    let major: u32 = std::env::var("CARGO_PKG_VERSION_MAJOR").ok().as_deref().and_then(|x| x.parse().ok()).expect("Failed to parse crate major version.");
    let minor: u32 = std::env::var("CARGO_PKG_VERSION_MINOR").ok().as_deref().and_then(|x| x.parse().ok()).expect("Failed to parse crate major version.");
    let patch: u32 = std::env::var("CARGO_PKG_VERSION_PATCH").ok().as_deref().and_then(|x| x.parse().ok()).expect("Failed to parse crate major version.");

    quote! {
        ::wings::marshal::Version {
            major: #major,
            minor: #minor,
            patch: #patch
        }
    }.into()
}

#[proc_macro_attribute]
pub fn export_type(_: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let original_item = TokenStream::from(item.clone());
    let item_type = parse_macro_input!(item as DeriveInput);
    let name = &item_type.ident;
    let name_str = name.to_string();

    quote! {
        #[derive(::wings::marshal::serde::Serialize, ::wings::marshal::serde::Deserialize)]
        #[serde(crate = "wings::marshal::serde")]
        #original_item

        impl ::wings::marshal::ExportType for #name {
            const TYPE: ::wings::marshal::StaticExportedType = ::wings::marshal::StaticExportedType {
                name: concat!( module_path!(), "::", #name_str ),
                version: ::wings::marshal::crate_version!()
            };
        }
    }.into()
}

#[proc_macro_attribute]
pub fn export_system(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let system_traits = get_system_traits(attr);
    let original_item = TokenStream::from(item.clone());

    let item_type = parse_macro_input!(item as DeriveInput);
    let name = &item_type.ident;
    let name_str = name.to_string();

    let export_function_identifier = crate_unique_id("__wings_describe");

    quote! {
        #original_item

        #(
            impl ::wings::marshal::SystemTrait for dyn #system_traits {
                const SYSTEM_TYPE: ::wings::marshal::StaticExportedType = <#name as ::wings::marshal::ExportType>::TYPE;
            }
        )*

        impl ::wings::marshal::ExportType for #name {
            const TYPE: ::wings::marshal::StaticExportedType = ::wings::marshal::StaticExportedType {
                name: concat!( module_path!(), "::", #name_str ),
                version: ::wings::marshal::crate_version!()
            };
        }

        const _: () = {
            #[allow(unused)]
            #[no_mangle]
            unsafe extern "C" fn #export_function_identifier () -> ::wings::marshal::GuestPointer {
                let mut descriptor = ::wings::marshal::system_descriptor_for::< #name >();
                
                #(
                    let sample_pointer = ::std::ptr::null::<#name>();
                    let v_table = ::std::mem::transmute::<_, [ * const (); 2]>(sample_pointer as * const dyn #system_traits)[1].into();

                    ::wings::marshal::add_system_descriptor_trait::< #name, dyn #system_traits >(
                        &mut descriptor,
                        v_table,
                        |system, func_index, buffer| ::wings::marshal::Proxyable::invoke(&mut *(*system.cast::<::std::cell::RefCell<dyn #system_traits>>()).borrow_mut(), func_index, buffer).expect("Failed to call proxy function"));
                )*

                ::wings::marshal::write_to_marshal_buffer(&descriptor)
            }
        };
    }.into()
}

#[proc_macro_attribute]
pub fn system_trait(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let host_request = is_host_request(attr);
    let original_trait = TokenStream::from(item.clone());
    let item_trait = parse_macro_input!(item as ItemTrait);

    let trait_name = item_trait.ident;
    let trait_name_string = trait_name.to_string();

    let host_export_system = if host_request {
        generate_host_export_system(&trait_name, &trait_name_string)
    }
    else {
        TokenStream::new()
    };

    let mut function_definitions = Vec::new();
    let mut function_invocations = Vec::new();
    for inner_item in item_trait.items {
        match inner_item {
            TraitItem::Fn(func_item) => {
                let (proxy_function, invocation) = generate_proxy_function_and_invocation(function_definitions.len() as u32, func_item);
                function_definitions.push(proxy_function);
                function_invocations.push(invocation);
            },
            _ => panic!("Non-function items are not supported in proxyable traits."),
        }
    }
    
    quote! {
        #original_trait

        const _: () = {
            impl ::wings::marshal::Proxyable for dyn #trait_name {
                type Proxy = TraitProxy;

                fn create_proxy(id: u32) -> Self::Proxy {
                    TraitProxy(id)
                }

                unsafe fn invoke(&mut self, func_index: u32, buffer: *mut Vec<u8>) -> ::std::result::Result<(), ::wings::WingsError> {
                    #(#function_invocations)*
                    Err(::wings::WingsError::InvalidFunction())
                }
            }

            pub struct TraitProxy(u32);

            impl #trait_name for TraitProxy {
                #(#function_definitions)*
            }

            impl ::std::ops::Deref for TraitProxy {
                type Target = dyn #trait_name;

                fn deref(&self) -> &Self::Target {
                    self
                }
            }

            impl ::std::ops::DerefMut for TraitProxy {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    self
                }
            }

            impl ::wings::marshal::ExportType for dyn #trait_name {
                const TYPE: ::wings::marshal::StaticExportedType = ::wings::marshal::StaticExportedType {
                    name: concat!( module_path!(), "::", #trait_name_string ),
                    version: ::wings::marshal::crate_version!()
                };
            }

            #host_export_system
        };
    }.into()
}

#[proc_macro]
pub fn instantiate_systems(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let system_trait_tuple = Punctuated::<Expr, Token![,]>::parse_terminated.parse(input)
        .expect("Failed to parse system traits");

    let [first, second] = system_trait_tuple.into_iter().collect::<Vec<_>>().try_into().ok().expect("Invalid number of arguments");
    let Expr::Path(group_path) = first else { panic!("Invalid group type") };
    let Expr::Array(systems) = second else { panic!("Invalid system array") };

    let system_paths = systems.elems.into_iter().map(|x| match x {
        Expr::Path(y) => y,
        _ => panic!("Expected system type identifier")
    }).collect::<Vec<_>>();

    let instantiate_id = crate_unique_id("__wings_instantiate");

    quote! {
        const _: () = {
            #[no_mangle]
            unsafe extern "C" fn #instantiate_id () -> ::wings::marshal::GuestPointer {
                let descriptor = ::wings::marshal::InstantiateGroup {
                    group_ty: <#group_path as ::wings::marshal::ExportType>::TYPE.into(),
                    systems: vec!( #( <#system_paths as ::wings::marshal::ExportType>::TYPE.into() , )* )
                };
                ::wings::marshal::write_to_marshal_buffer(&descriptor)
            }
        };
    }.into()
}

fn crate_unique_id(prefix: &str) -> Ident {
    static UNIQUE_ID: AtomicUsize = AtomicUsize::new(0);

    let crate_name = std::env::var("CARGO_CRATE_NAME").expect("Failed to get crate name");
    Ident::new(&format!("{prefix}_{}_{crate_name}_{}", crate_name.len(), UNIQUE_ID.fetch_add(1, Ordering::Relaxed)), Span::call_site())
}

fn is_host_request(attr: proc_macro::TokenStream) -> bool {
    if attr.is_empty() {
        false
    }
    else {
        let ident_name = parse::<Ident>(attr).expect("Failed to parse attribute parameter.").to_string();
        assert!(ident_name == "host", "Invalid attribute parameter '{}'", ident_name);
        true
    }
}

fn generate_host_export_system(trait_name: &Ident, trait_name_string: &str) -> TokenStream {
    quote! {
        impl ::wings::marshal::SystemTrait for dyn #trait_name {
            const SYSTEM_TYPE: ::wings::marshal::StaticExportedType = ::wings::marshal::StaticExportedType {
                name: concat!( module_path!(), "::", #trait_name_string ),
                version: ::wings::marshal::crate_version!()
            };
        }
    }
}

fn generate_proxy_function_and_invocation(index: u32, func_item: TraitItemFn) -> (TokenStream, TokenStream) {
    let func_args = get_non_receiver_args(&func_item);
    let proxy_function = generate_proxy_function(index, &func_item, &func_args);
    let invocation = generate_proxy_invocation(index, &func_item, &func_args);

    (proxy_function, invocation)
}

fn generate_proxy_function(index: u32, func_item: &TraitItemFn, args: &FuncArgs) -> TokenStream {
    let rebind_arguments = (0..args.original_args.len()).map(|x| generate_rebind_argument(x, args)).collect::<Vec<_>>();
    let lower_arguments = args.index_args.iter().map(generate_lower_argument).collect::<Vec<_>>();
    let lift_results = (0..args.original_args.len()).map(|x| generate_lift_result(x, args)).collect::<Vec<_>>();
    let mut signature = TokenStream::new();
    func_item.sig.to_tokens(&mut signature);

    quote! {
        #signature {
            unsafe {
                #(#rebind_arguments)*
                let (pointer, size) = {
                    let mut section_writer = ::wings::marshal::SectionedBufferWriter::from_marshal_buffer();
                    #(#lower_arguments)*
                    let buffer = section_writer.into_inner();
                    (buffer.as_mut_ptr(), buffer.len() as u32)
                };
                ::wings::marshal::__wings_invoke_proxy_function(self.0, #index, pointer.into(), size);
                {
                    let mut section_reader = ::wings::marshal::SectionedBufferReader::from_marshal_buffer();
                    #(#lift_results)*
                    ::wings::marshal::bincode::deserialize(section_reader.section().expect("Wings failed to lift result.")).expect("Wings failed to lift result.")
                }
            }
        }
    }
}

fn generate_proxy_invocation(index: u32, func_item: &TraitItemFn, args: &FuncArgs) -> TokenStream {
    let func_name = &func_item.sig.ident;
    let lifted_arguments = (0..args.original_args.len()).map(|x| generate_lift_argument(x, args)).collect::<Vec<_>>();
    let made_temporaries = (0..args.original_args.len()).map(|x| generate_make_temporary(x, args)).collect::<Vec<_>>();
    let lowered_results = (0..args.original_args.len()).map(|x| generate_lower_result(x, args)).collect::<Vec<_>>();

    quote! {
        if func_index == #index {
            let mut section_reader = ::wings::marshal::SectionedBufferReader::new(&mut *buffer);
            #(#lifted_arguments)*
            drop(section_reader);
            let result = self.#func_name ( #(#made_temporaries)* );
            let mut section_writer = ::wings::marshal::SectionedBufferWriter::new(&mut *buffer);
            #(#lowered_results)*
            return ::wings::marshal::bincode::serialize_into(section_writer.section(), &result).map_err(::wings::WingsError::Serialization);
        }
    }
}

fn get_non_receiver_args(func_item: &TraitItemFn) -> FuncArgs {
    let mut arg_types = Vec::new();
    let mut index_args = Vec::new();
    let mut original_args = Vec::new();

    for (index, input) in func_item.sig.inputs.iter().enumerate().skip(1) {
        match input {
            FnArg::Receiver(_) => panic!("Receiver in unexpected position."),
            FnArg::Typed(x) => match &*x.pat {
                Pat::Ident(id) => {
                    arg_types.push(&*x.ty);
                    index_args.push(Ident::new(&format!("_arg{}", index), Span::call_site()));
                    original_args.push(&id.ident);
                },
                _ => panic!("Pattern bindings in proxy traits are unsupported.")
            }
        }
    }
    
    FuncArgs {
        arg_types,
        index_args,
        original_args
    }
}

fn generate_rebind_argument(index: usize, args: &FuncArgs) -> TokenStream {
    let original_ident = args.original_args[index];
    let index_ident = &args.index_args[index];
    let reference = matches!(args.arg_types[index], Type::Reference(_));

    if reference {
        quote! {
            let mut #index_ident = #original_ident;
        }
    }
    else {
        quote! {
            let mut #index_ident = &#original_ident;
        }
    }
}

fn generate_lower_argument(identifier: &Ident) -> TokenStream {
    quote! {
        ::wings::marshal::MarshalAs::lower_argument(&#identifier, section_writer.section()).expect("Wings failed to lower argument.");
    }
}

fn generate_lift_argument(index: usize, args: &FuncArgs) -> TokenStream {
    let identifier = &args.index_args[index];
    let ty = &args.arg_types[index];
    let reference = matches!(args.arg_types[index], Type::Reference(_));

    if reference {
        quote! {
            let mut #identifier = <#ty as ::wings::marshal::MarshalAs<_>>::lift_argument(section_reader.section()?)?;
        }
    }
    else {
        quote! {
            let mut #identifier = <&#ty as ::wings::marshal::MarshalAs<_>>::lift_argument(section_reader.section()?)?;
        }
    }
}

fn generate_lift_result(index: usize, args: &FuncArgs) -> TokenStream {
    let identifier = &args.index_args[index];
    let reference = matches!(args.arg_types[index], Type::Reference(_));

    if reference {
        quote! {
            ::wings::marshal::MarshalAs::lift_result(&mut #identifier, section_reader.section().expect("Wings failed to get argument result section.")).expect("Wings failed to lift argument result.");
        }
    }
    else {
        quote! {}
    }
}

fn generate_lower_result(index: usize, args: &FuncArgs) -> TokenStream {
    let identifier = &args.index_args[index];
    let ty = &args.arg_types[index];
    let reference = matches!(args.arg_types[index], Type::Reference(_));

    if reference {
        quote! {
            <#ty as ::wings::marshal::MarshalAs<_>>::lower_result(&#identifier, section_writer.section())?;
        }
    }
    else {
        quote! { }
    }
}

fn generate_make_temporary(index: usize, args: &FuncArgs) -> TokenStream {
    let identifier = &args.index_args[index];
    let ty = &args.arg_types[index];
    let reference = matches!(args.arg_types[index], Type::Reference(_));

    if reference {
        quote! {
            <#ty as ::wings::marshal::MarshalAs<_>>::make_temporary(&mut #identifier),
        }
    }
    else {
        quote! {
            #identifier,
        }
    }
}

fn get_system_traits(attr: proc_macro::TokenStream) -> Vec<ExprPath> {
    let system_trait_tuple = Punctuated::<Expr, Token![,]>::parse_terminated.parse(attr)
        .expect("Failed to parse system traits");

    system_trait_tuple.into_iter().map(|x| match x {
        Expr::Path(y) => y,
        _ => panic!("Expected system trait name"),
    }).collect()
}

struct FuncArgs<'a> {
    pub arg_types: Vec<&'a Type>,
    pub index_args: Vec<Ident>,
    pub original_args: Vec<&'a Ident>,
}