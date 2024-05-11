use wings::*;

#[export_type]
pub struct Client;

#[system_trait(host)]
pub trait ExampleSystem: 'static {
    fn get_value(&self) -> u32;
    fn set_and_double(&mut self, value: &mut u32);
    fn print(&self, value: &str);
}

pub mod on {
    use super::*;

    #[export_type]
    pub struct ExampleEvent {
        pub value: u32,
    }
}
