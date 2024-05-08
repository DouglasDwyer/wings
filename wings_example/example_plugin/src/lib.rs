use example_host_system::*;
use wings::*;

instantiate_systems!(Client, [
    PluginSystem
]);


#[export_system]
pub struct PluginSystem {
    ctx: WingsContextHandle<Self>
}

impl PluginSystem {
    fn handle_event(&mut self, event: &example_host_system::on::ExampleEvent) {
        self.ctx.get::<dyn ExampleSystem>().print(&format!("Handle event {}", event.value));
    }
}

impl WingsSystem for PluginSystem {
    const DEPENDENCIES: Dependencies = dependencies()
        .with::<dyn ExampleSystem>();

    const EVENT_HANDLERS: EventHandlers<Self> = event_handlers()
        .with(Self::handle_event);

    fn new(mut ctx: WingsContextHandle<Self>) -> Self {
        let mut my_val = 22;
        ctx.get_mut::<dyn ExampleSystem>().set_and_double(&mut my_val);
        ctx.raise_event(example_host_system::on::ExampleEvent { value: 10112 });
        ctx.get::<dyn ExampleSystem>().print(&format!("The new value was {}", my_val));

        Self {
            ctx
        }
    }
}

impl Drop for PluginSystem {
    fn drop(&mut self) {
        self.ctx.get::<dyn ExampleSystem>().print(&format!("The system was dropped"));
    }
}