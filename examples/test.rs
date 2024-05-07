use example_host_system::*;
use geese::*;
use wings_host::*;

pub struct TestHost;

impl Host for TestHost {
    const EVENTS: Events<Self> = events()
        .with::<example_host_system::on::ExampleEvent>();

    const SYSTEMS: Systems<Self> = systems()
        .with::<ExampleSystemImpl>(traits()
            .with::<dyn example_host_system::ExampleSystem>());

    type Engine = wasmi::Engine;

    fn create_engine(_: &mut GeeseContextHandle<WingsHost<Self>>) -> Self::Engine {
        wasmi::Engine::new(&wasmi::Config::default())
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
        println!("Raised !! {}", event.value);
    }
}

impl ExampleSystem for ExampleSystemImpl {
    fn get_value(&self) -> u32 {
        println!("getit");
        self.value
    }

    fn set_and_double(&mut self, value: &mut u32) {
        println!("setit");
        self.value = *value;
        *value *= 2;
    }

    fn print(&self, value: &str) {
        println!("Plugin says {value}");
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
    let mut ctx = GeeseContext::default();
    ctx.flush()
        .with(geese::notify::add_system::<WingsHost<TestHost>>());

    let mut host = ctx.get_mut::<WingsHost<TestHost>>();
    let module = host.load(&include_bytes!("../target/wasm32-unknown-unknown/debug/example_plugin.wasm")[..]).unwrap();
    
    let mut image = WingsImage::default();
    image.add::<example_host_system::Client>(&module);

    host.instantiate(&image).unwrap();

    drop(host);
    ctx.flush();
}