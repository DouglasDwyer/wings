use example_host_system::*;
use geese::*;
use wings_host::*;

include!(concat!(env!("OUT_DIR"), "/example_plugin.rs"));

pub struct TestHost;

impl Host for TestHost {
    const EVENTS: Events<Self> = events()
        .with::<example_host_system::on::ExampleEvent>();

    const SYSTEMS: Systems<Self> = systems()
        .with::<ExampleSystemImpl>(traits()
            .with::<dyn example_host_system::ExampleSystem>());

    type Engine = wasmi_runtime_layer::Engine;

    fn create_engine(_: &mut GeeseContextHandle<WingsHost<Self>>) -> Self::Engine {
        wasmi_runtime_layer::Engine::default()
    }
}

pub struct ExampleSystemImpl {
    value: u32
}

impl ExampleSystemImpl {
    fn handle_error(&mut self, event: &wings_host::on::Error) {
        println!("Error occurred: {}", event.error);
    }

    fn on_example_event(&mut self, event: &example_host_system::on::ExampleEvent) {
        println!("Received an event from the guest: {}", event.value);
    }
}

impl ExampleSystem for ExampleSystemImpl {
    fn get_value(&self) -> u32 {
        println!("Get the value from the host");
        self.value
    }

    fn set_and_double(&mut self, value: &mut u32) {
        println!("Set the value and double it on the host");
        self.value = *value;
        *value *= 2;
    }

    fn print(&self, value: &str) {
        println!("Plugin says '{value}'");
    }
}

impl AsMut<dyn ExampleSystem> for ExampleSystemImpl {
    fn as_mut(&mut self) -> &mut dyn ExampleSystem {
        self
    }
}

impl GeeseSystem for ExampleSystemImpl {
    const EVENT_HANDLERS: EventHandlers<Self> = event_handlers()
        .with(Self::handle_error)
        .with(Self::on_example_event);

    fn new(_: GeeseContextHandle<Self>) -> Self {
        Self {
            value: 0
        }
    }
}

fn main() {
    // Create an event system context
    let mut ctx = GeeseContext::default();

    // Add the host system
    ctx.flush()
        .with(geese::notify::add_system::<WingsHost<TestHost>>());

    // Get the host system
    let mut host = ctx.get_mut::<WingsHost<TestHost>>();

    // Load a module
    let module = host.load(EXAMPLE_PLUGIN_WASM).unwrap();
    
    // Create an image of all the modules to instantiate
    let mut image = WingsImage::default();
    image.add::<example_host_system::Client>(&module);

    // Link and instantiate the systems contained in the given image
    host.instantiate(&image);
    drop(host);
    
    // Process all events raised during instantiation
    ctx.flush();
}