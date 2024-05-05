use example_host_system::*;
use wings::*;

instantiate_systems!(Client, [
    Crunk
]);

#[system_trait]
pub trait ThePublic {}

#[system_trait]
pub trait TheSkunk {}

#[export_system(ThePublic)]
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

impl WingsSystem for Crunk {
    const DEPENDENCIES: Dependencies = dependencies()
        .with::<dyn ExampleSystem>()
        .with::<dyn TheSkunk>();

    const EVENT_HANDLERS: EventHandlers<Self> = event_handlers()
        .with(Self::handle_event);

    fn new(ctx: WingsContextHandle<Self>) -> Self {
        unsafe { wings::marshal::__wings_dbg(2); }
        Self {
            ctx
        }
    }
}

#[export_system(TheSkunk)]
pub struct Skunk;

impl TheSkunk for Skunk {}

impl WingsSystem for Skunk {
    fn new(_: WingsContextHandle<Self>) -> Self {
        unsafe { wings::marshal::__wings_dbg(1); }
        Self
    }
}