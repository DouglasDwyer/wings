use example_host_system::*;
use wings::*;

instantiate_systems!(Client, [
    Crunk
]);

#[system_trait]
pub trait ThePublic {}

#[system_trait]
pub trait TheSkunk {
}

#[export_system(ThePublic)]
pub struct Crunk {
    ctx: WingsContextHandle<Self>
}

impl Crunk {
    fn handle_event(&mut self, event: &example_host_system::on::ExampleEvent) {
        self.ctx.get::<dyn ExampleSystem>().print(&format!("Handle event {}", event.value));
    }
}

impl ThePublic for Crunk {}

impl WingsSystem for Crunk {
    const DEPENDENCIES: Dependencies = dependencies()
        .with::<dyn ExampleSystem>()
        .with::<dyn TheSkunk>();

    const EVENT_HANDLERS: EventHandlers<Self> = event_handlers()
        .with(Self::handle_event);

    fn new(mut ctx: WingsContextHandle<Self>) -> Self {
        let mut my_val = 22;
        ctx.get_mut::<dyn ExampleSystem>().set_and_double(&mut my_val);
        ctx.raise_event(example_host_system::on::ExampleEvent { value: 10112 });
        ctx.get::<dyn ExampleSystem>().print(&format!("Your mom lol {}", my_val));

        Self {
            ctx
        }
    }
}

impl Drop for Crunk {
    fn drop(&mut self) {
        self.ctx.get::<dyn ExampleSystem>().print(&format!("oops drop it"));
    }
}

#[export_system(TheSkunk)]
pub struct Skunk;

impl TheSkunk for Skunk {
}

impl WingsSystem for Skunk {
    fn new(_: WingsContextHandle<Self>) -> Self {
        Self
    }
}