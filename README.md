# wings

[![Crates.io](https://img.shields.io/crates/v/wings.svg)](https://crates.io/crates/wings)
[![Docs.rs](https://docs.rs/wings/badge.svg)](https://docs.rs/wings)

Wings is a WASM plugin system for Rust. It integrates directly with the [Geese event library](https://github.com/DouglasDwyer/geese), allowing
plugins to seamlessly communicate with one another and the host using events and systems. The following features are supported:

- *Sending events to the host*: Guests can send and receive strongly-typed events to the host, which will be injected into the host's Geese context.
- *Sending events to other guest systems*: Events will propagate between guest systems and from the host's Geese context into WASM modules.
- *Accessing systems of the host*: Hosts can expose Geese systems as trait objects, which can be called directly from the guest. Function arguments and return types are serialized across the WASM boundary.
- *Accessing systems of other plugins*: Plugins can export their systems as trait objects, which other plugins can call. Just like regular Geese, Wings guarantees that every system is a singleton - if separate plugins are loaded that share a dependency, both plugins will access the same dependency instance.
- *Automatically resolving plugin dependencies/versions*: Plugins are built as Rust crates with all transitive dependencies included, so there's no need to worry about creating a dependency resolver. Whenever Wings loads a system, it will choose the newest Semver-compatible version from the set of loaded plugins.

### Example

The following is an abridged example of how to use Wings. The complete code may be found in the [`wings_example` folder](/wings_example/).

Wings allows for defining traits like the following, which can be shared between the code of hosts and between plugins:

```rust
// Define a system that the host will expose.
// This can also be used to expose guest systems to other guest systems.
#[system_trait(host)]
pub trait ExampleSystem: 'static {
    // Prints a value to the console.
    fn print(&self, value: &str);
}
```

Then, the system may be referenced as a trait object from WASM:

```rust
// Define a Wings system that will run within WASM
#[export_system]
pub struct PluginSystem;

impl WingsSystem for PluginSystem {
    // Declare a dependency on the exported host system
    const DEPENDENCIES: Dependencies = dependencies()
        .with::<dyn ExampleSystem>();

    // Invoked when the plugin is created
    fn new(mut ctx: WingsContextHandle<Self>) -> Self {
        // Get the system from WASM and invoke its function
        ctx.get::<dyn ExampleSystem>().print(&format!("Hello from WASM!"));
        Self
    }
}
```

Finally, the system may be implemented on the host and exposed to plugins:

```rust
// Define a host type
pub struct TestHost;

impl Host for TestHost {
    // Declare the systems that should be exported to WASM,
    // and the traits under which they should be exported
    const SYSTEMS: Systems<Self> = systems()
        .with::<ExampleSystemImpl>(traits()
            .with::<dyn example_host_system::ExampleSystem>());

    ...
}

// Declare an implementation for the WASM-exported system
pub struct ExampleSystemImpl;

impl ExampleSystem for ExampleSystemImpl {
    fn print(&self, value: &str) {
        println!("Plugin says '{value}'");
    }
}
```

In general, anything possible with vanilla Geese is also possible with Wings. See the [example](/wings_example/) for demonstration of more functionality.