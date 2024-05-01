use proc_macro2::*;
use quote::*;
use syn::*;

#[proc_macro_attribute]
pub fn proxyable(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let original_trait = TokenStream::from(item.clone());
    let item_trait = parse_macro_input!(item as ItemTrait);

    let trait_name = item_trait.ident;

    let mut function_definitions = Vec::new();
    let mut function_invocations = Vec::new();
    for inner_item in item_trait.items {
        match inner_item {
            TraitItem::Fn(func_item) => {
                let (proxy_function, invocation) = generate_proxy_function_and_invocation(function_definitions.len(), func_item);
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

                fn invoke(&mut self, func_index: u32, buffer: &mut Vec<u8>) -> ::std::result::Result<(), ::wings::WingsError> {
                    unsafe {
                        #(#function_invocations)*
                        Err(::wings::WingsError::InvalidFunction())
                    }
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
        };
    }.into()
}

fn generate_proxy_function_and_invocation(index: usize, func_item: TraitItemFn) -> (TokenStream, TokenStream) {
    let func_args = get_non_receiver_args(&func_item);
    let proxy_function = generate_proxy_function(&func_item, &func_args.index_args, &func_args.original_args);
    let invocation = generate_proxy_invocation(index, &func_item, &func_args);

    (proxy_function, invocation)
}

fn generate_proxy_function(func_item: &TraitItemFn, index_args: &[Ident], original_args: &[&Ident]) -> TokenStream {
    let rebind_arguments = index_args.iter().zip(original_args.iter().copied()).map(generate_rebind_argument).collect::<Vec<_>>();
    let lower_arguments = index_args.iter().map(generate_lower_argument).collect::<Vec<_>>();
    let lift_results = index_args.iter().map(generate_lift_result).collect::<Vec<_>>();
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
                    (buffer.as_mut_ptr() as u32, buffer.len() as u32)
                };
                ::wings::marshal::__wings_invoke_host(pointer, size);
                {
                    let mut section_reader = ::wings::marshal::SectionedBufferReader::from_marshal_buffer();
                    #(#lift_results)*
                    ::wings::marshal::bincode::deserialize(section_reader.section().expect("Wings failed to lift result.")).expect("Wings failed to lift result.")
                }
            }
        }
    }
}

fn generate_proxy_invocation(index: usize, func_item: &TraitItemFn, args: &FuncArgs) -> TokenStream {
    let func_name = &func_item.sig.ident;
    let lifted_arguments = args.index_args.iter().zip(args.arg_types.iter().copied()).map(generate_lift_argument);
    let made_temporaries = args.index_args.iter().zip(args.arg_types.iter().copied()).map(generate_make_temporary);
    let lowered_results = args.index_args.iter().zip(args.arg_types.iter().copied()).map(generate_lower_result);
    let id = index as u32;

    quote! {
        if func_index == #id {
            let mut section_reader = ::wings::marshal::SectionedBufferReader::new(buffer);
            #(#lifted_arguments)*
            let result = self.#func_name ( #(#made_temporaries)* );
            drop(section_reader);
            let mut section_writer = ::wings::marshal::SectionedBufferWriter::new(buffer);
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

fn generate_rebind_argument((index_ident, original_ident): (&Ident, &Ident)) -> TokenStream {
    quote! {
        let mut #index_ident = #original_ident;
    }
}

fn generate_lower_argument(identifier: &Ident) -> TokenStream {
    quote! {
        ::wings::marshal::MarshalAs::lower_argument(&#identifier, section_writer.section()).expect("Wings failed to lower argument.");
    }
}

fn generate_lift_argument((identifier, ty): (&Ident, &Type)) -> TokenStream {
    quote! {
        let mut #identifier = <#ty as ::wings::marshal::MarshalAs<_>>::lift_argument(section_reader.section()?)?;
    }
}

fn generate_lift_result(identifier: &Ident) -> TokenStream {
    quote! {
        ::wings::marshal::MarshalAs::lift_result(&mut #identifier, section_reader.section().expect("Wings failed to get argument result section.")).expect("Wings failed to lift argument result.");
    }
}

fn generate_lower_result((identifier, ty): (&Ident, &Type)) -> TokenStream {
    quote! {
        <#ty as ::wings::marshal::MarshalAs<_>>::lower_result(&#identifier, section_writer.section())?;
    }
}

fn generate_make_temporary((identifier, ty): (&Ident, &Type)) -> TokenStream {
    quote! {
        <#ty as ::wings::marshal::MarshalAs<_>>::make_temporary(&mut #identifier),
    }
}

struct FuncArgs<'a> {
    pub arg_types: Vec<&'a Type>,
    pub index_args: Vec<Ident>,
    pub original_args: Vec<&'a Ident>,
}