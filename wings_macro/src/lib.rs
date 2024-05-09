#![deny(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]

//! Defines macros for declaring traits and systems in [`wings`](https://github.com/DouglasDwyer/wings).

use proc_macro2::*;
use quote::*;
use std::sync::atomic::*;
use syn::*;
use syn::parse::*;
use syn::punctuated::*;

/// Creates a `wings_marshal::Version` object describing the current version of this crate.
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

/// Exports the provided type, allowing it to be used as an event or system type.
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

/// Exports the provided system, allowing `wings` to instantiate it and other systems to access it.
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

/// Marks this trait as an interface through which plugins can access a system.
#[proc_macro_attribute]
pub fn system_trait(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let host_request = is_host_request(attr);
    let mut item_trait = parse_macro_input!(item as ItemTrait);

    let trait_name = item_trait.ident.clone();
    let trait_name_string = trait_name.to_string();

    let host_export_system = if host_request {
        generate_host_export_system(&trait_name, &trait_name_string)
    }
    else {
        TokenStream::new()
    };

    let mut function_definitions = Vec::new();
    let mut function_invocations = Vec::new();
    let mut function_globals = Vec::new();

    for inner_item in &mut item_trait.items {
        match inner_item {
            TraitItem::Fn(func_item) => {
                let generated = generate_proxy_function_implementations(function_definitions.len() as u32, &trait_name, func_item);
                function_globals.push(generated.global);
                function_invocations.push(generated.invoker);
                function_definitions.push(generated.proxy);
            },
            _ => panic!("Non-function items are not supported in proxyable traits."),
        }
    }

    if !host_request {
        panic!("Cannot define global functions for non-host systems.");
    }
    
    quote! {
        #item_trait

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

        #(#function_globals)*
    }.into()
}

/// Instructs the host to instantiate systems (and all of their dependencies)
/// when creating the given system group.
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

/// Gets an identifier which is completely unique based upon the crate name.
fn crate_unique_id(prefix: &str) -> Ident {
    /// An ID that uniquely identifies procedurally-generated items in this crate.
    static UNIQUE_ID: AtomicUsize = AtomicUsize::new(0);

    let crate_name = std::env::var("CARGO_CRATE_NAME").expect("Failed to get crate name");
    Ident::new(&format!("{prefix}_{}_{crate_name}_{}", crate_name.len(), UNIQUE_ID.fetch_add(1, Ordering::Relaxed)), Span::call_site())
}

/// Determines based upon arguments to `system_trait` whether this is a host trait.
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

/// Generates the `SystemTrait` implementation for a host-exported system trait.
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

/// Generates the proxy function implementation and proxy invocation function
/// for the given function item.
fn generate_proxy_function_implementations(index: u32, trait_name: &Ident, func_item: &mut TraitItemFn) -> ProxyFunction {
    let global_name = remove_global_attribute(func_item);
    let func_args = get_non_receiver_args(func_item);
    let global = generate_global_function(trait_name, func_item, &func_args, global_name);
    let proxy = generate_proxy_function(index, func_item, &func_args);
    let invoker = generate_proxy_invocation(index, func_item, &func_args);

    ProxyFunction {
        global,
        invoker,
        proxy,
    }
}

/// Generates a proxy function which can be called globally, without the use of a trait object.
fn generate_global_function(trait_name: &Ident, func_item: &TraitItemFn, args: &FuncArgs, global_name: Option<Ident>) -> TokenStream {
    if let Some(ident) = global_name {
        let mut func_signature = func_item.sig.clone();

        func_signature.inputs = func_signature.inputs.clone().into_iter().skip(1).collect();
        func_signature.ident = ident;
        
        let mut signature = TokenStream::new();
        func_signature.to_tokens(&mut signature);
    
        let func_ident = &func_item.sig.ident;
        let arg_names = &args.original_args;

        quote! {
            pub #func_signature {
                unsafe {
                    <dyn #trait_name as ::wings::marshal::Proxyable>::create_proxy(::wings::marshal::proxy_index::<dyn #trait_name>())
                        . #func_ident( #( #arg_names )* )
                }
            }
        }
    }
    else {
        quote! {}
    }
}

/// Generates a proxy function for remotely calling a trait item.
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

/// Generates an invocation function for locally responding to a remote call.
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

/// Collates the arguments to the provided function, excluding the `self` argument.
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

/// Generates code to rebind the given argument using a unique identifier.
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

/// Generates code to lower the given identifier into a section writer.
fn generate_lower_argument(identifier: &Ident) -> TokenStream {
    quote! {
        ::wings::marshal::Marshal::lower_argument(&#identifier, section_writer.section()).expect("Wings failed to lower argument.");
    }
}

/// Generates code to lift the given argument from a section reader.
fn generate_lift_argument(index: usize, args: &FuncArgs) -> TokenStream {
    let identifier = &args.index_args[index];
    let ty = &args.arg_types[index];
    let reference = matches!(args.arg_types[index], Type::Reference(_));

    if reference {
        quote! {
            let mut #identifier = <#ty as ::wings::marshal::Marshal>::lift_argument(section_reader.section()?)?;
        }
    }
    else {
        quote! {
            let mut #identifier = <&#ty as ::wings::marshal::Marshal>::lift_argument(section_reader.section()?)?;
        }
    }
}

/// Generates code to lift the given result argument from a section reader.
fn generate_lift_result(index: usize, args: &FuncArgs) -> TokenStream {
    let identifier = &args.index_args[index];
    let reference = matches!(args.arg_types[index], Type::Reference(_));

    if reference {
        quote! {
            ::wings::marshal::Marshal::lift_result(&mut #identifier, section_reader.section().expect("Wings failed to get argument result section.")).expect("Wings failed to lift argument result.");
        }
    }
    else {
        quote! {}
    }
}

/// Generates code to lower the given result argument into a section writer.
fn generate_lower_result(index: usize, args: &FuncArgs) -> TokenStream {
    let identifier = &args.index_args[index];
    let ty = &args.arg_types[index];
    let reference = matches!(args.arg_types[index], Type::Reference(_));

    if reference {
        quote! {
            <#ty as ::wings::marshal::Marshal>::lower_result(&#identifier, section_writer.section())?;
        }
    }
    else {
        quote! { }
    }
}

/// Generates code to create a temporary from the a stack-allocated value.
fn generate_make_temporary(index: usize, args: &FuncArgs) -> TokenStream {
    let identifier = &args.index_args[index];
    let ty = &args.arg_types[index];
    let reference = matches!(args.arg_types[index], Type::Reference(_));

    if reference {
        quote! {
            <#ty as ::wings::marshal::Marshal>::make_temporary(&mut #identifier),
        }
    }
    else {
        quote! {
            #identifier,
        }
    }
}

/// Gets the set of exported traits associated with a system.
fn get_system_traits(attr: proc_macro::TokenStream) -> Vec<ExprPath> {
    let system_trait_tuple = Punctuated::<Expr, Token![,]>::parse_terminated.parse(attr)
        .expect("Failed to parse system traits");

    system_trait_tuple.into_iter().map(|x| match x {
        Expr::Path(y) => y,
        _ => panic!("Expected system trait name"),
    }).collect()
}

/// Removes the `global` attribute macro from a trait function item.
fn remove_global_attribute(item: &mut TraitItemFn) -> Option<Ident> {
    let mut found_index = None;
    for (index, attr) in item.attrs.iter().enumerate() {
        if attr.path().get_ident().map(|x| *x == "global").unwrap_or(false) {
            assert!(found_index.replace(index).is_none(), "Cannot have duplicate 'global' attributes on a single function");
        }
    }

    if let Some(index) = found_index {
        let attr = item.attrs.remove(index);
        if let Ok(ident) = attr.parse_args::<Ident>() {
            Some(ident)
        }
        else if attr.meta.require_path_only().is_ok() {
            Some(item.sig.ident.clone())
        }
        else {
            panic!("Global name not specified")
        }
    }
    else {
        None
    }
}

/// The arguments to a trait function.
struct FuncArgs<'a> {
    /// The types of the arguments.
    pub arg_types: Vec<&'a Type>,
    /// The arguments as relabeled, unique index-based identifiers.
    pub index_args: Vec<Ident>,
    /// The original argument identifiers.
    pub original_args: Vec<&'a Ident>,
}

/// Holds the token streams of procedurally-generated functions for a proxy object.
struct ProxyFunction {
    /// The tokens for invoking the proxy function globally, if any.
    global: TokenStream,
    /// The tokens for locally invoking the underlying function.
    invoker: TokenStream,
    /// The tokens for requesting a remote proxy call.
    proxy: TokenStream,
}