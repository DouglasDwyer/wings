use example_host_system::*;
use wings::*;

#[system_trait]
pub trait ThePublic {}

#[system_trait]
pub trait BhePublic {}

#[export_system]
pub struct Crunk {
    ctx: WingsContextHandle<Self>
}

impl Crunk {
    fn handle_event(&mut self, _: &example_host_system::on::ExampleEvent) {
        let test = self.ctx.get_mut::<dyn ExampleSystem>();
        let mut x = 2;
        test.set_and_double(&mut x);
        x = test.get_value() / 2;
        test.set_and_double(&mut x);
    }
}

impl ThePublic for Crunk {}
impl BhePublic for Crunk {}

impl WingsSystem for Crunk {
    const DEPENDENCIES: Dependencies = dependencies()
        .with::<dyn ExampleSystem>();

    const EVENT_HANDLERS: EventHandlers<Self> = event_handlers()
        .with(Self::handle_event);

    fn new(ctx: WingsContextHandle<Self>) -> Self {
        Self {
            ctx
        }
    }
}