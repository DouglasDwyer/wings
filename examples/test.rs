use wings::*;

#[proxyable]
pub trait Test {
    fn best(&mut self, x: &mut i32) -> i32;
}

pub struct ProxyImplementation;

impl Test for ProxyImplementation {
    fn best(&mut self, x: &mut i32) -> i32 {
        let result = *x + 1;
        *x *= 2;
        result
    }
}

fn main() {
    let mut proxy = <dyn Test as wings::marshal::Proxyable>::create_proxy(29);

    let mut x = 7;

    println!("Hi {x} {:?}", proxy.best(&mut x));
}

#[no_mangle]
unsafe extern "C" fn __wings_invoke_host(pointer: u32, size: u32) -> u32 {
    let mut imple = ProxyImplementation;
    <dyn Test as wings::marshal::Proxyable>::invoke(&mut imple, 0, &mut wings::marshal::MARSHAL_BUFFER).unwrap();
    wings::marshal::MARSHAL_BUFFER.as_mut_ptr() as u32
}