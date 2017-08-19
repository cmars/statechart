# statechart
[![Build Status](https://travis-ci.org/cmars/statechart.svg?branch=master)](https://travis-ci.org/cmars/statechart)
[![Crate](https://img.shields.io/crates/v/statechart.svg)](https://crates.io/crates/statechart)

A rust implementation of statecharts. Statecharts are a visual approach to expressing hierarchical state machines, useful for describing reactive, event-driven systems. Statecharts were originally introduced in [Harel87](http://www.wisdom.weizmann.ac.il/~dharel/SCANNED.PAPERS/Statecharts.pdf). [Harel07](http://www.wisdom.weizmann.ac.il/~harel/papers/Statecharts.History.pdf) gives background and context to the innovation and development of statecharts. For technical context, application to MDSD and practical systems design, see [_D. Harel and M. Politi, Modeling Reactive Systems with Statecharts: The STATEMATE Approach, (with M. Politi)_](http://www.wisdom.weizmann.ac.il/~harel/reactive_systems.html).

## Current project focus

This crate initially aims to provide a statechart document model and interpreter, influenced by, and probably mostly isomorphic to the concepts, entities and behaviors described in the [W3C State Chart XML specification](https://www.w3.org/TR/scxml/). However, the focus is not full scxml compliance, but the following use-cases:

- Rust macros for expressing and executing statecharts from Rust code directly.
- Interpreter compatibility with futures & streams to support asynchronous execution.
- Automating some useful, operational tasks with software agents, expressed as statecharts.
  - Personal assistants
  - Automated software operations, auto-{healing,scaling,alerting,reconfiguring}
  - Privacy and security, "situational awareness" for networked applications & devices
- Performance, after usefulness and correctness

## Possible areas of further interest

Applications which I am not actively developing for yet, but could be interesting
and useful contributions:

- `no_std` support for embedded applications. The kind of thing statecharts were invented for!
- A compiler backend. Target LLVM or an FPGA!
- SCXML compatibility, where it doesn't conflict with the above goals, and there's a good use for the angle brackets.

---

Copyright 2017 Casey Marshall

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
