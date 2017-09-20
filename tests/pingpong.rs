#[cfg(test)]

extern crate env_logger;

#[macro_use]
extern crate statechart;

use std::rc::Rc;

use statechart::ast::*;
use statechart::interpreter::*;

#[test]
fn pingpong() {
    let _ = env_logger::init();
    let sc = states!{ root {
        initial_label: Some("init".to_string()),
        substates: [
            state!{ init {
                on_entry: [
                    action_assign!(key: "i", value: Value::Int(0)),
                    action_raise!(topic: "ping", contents: Value::Int(1))],
                transitions: [goto!(target: ping)]}},
            state!{ ping {
                transitions: [
                    goto!(target: pong, topics: ["ping"],
                        cond: cond_fn!(|it: &Interpreter| {
                            match it.get_var("i") {
                                Some(&Value::Int(i)) => i < 10,
                                _ => false,
                            }}),
                        actions: [
                            action_raise!(topic: "pong"),
                            action_fn!(Rc::new(|it: &mut Interpreter| {
                                let i = match it.get_var("i") {
                                    Some(&Value::Int(i)) => i,
                                    _ => return Err(Fault::ActionError(
                                        "i: invalid or uninitialized value".to_string())),
                                };
                                it.set_var("i", Value::Int(i+1));
                                Ok(())
                            }))]),
                    goto!(target: end,
                        cond: cond_fn!(|it: &Interpreter| {
                            match it.get_var("i") {
                                Some(&Value::Int(i)) => i >= 10,
                                _ => false,
                            }
                        })),
                ],
                on_entry: [action_log!(message: "ping!")],
            }},
            state!{ pong {
                transitions: [goto!(target: ping, topics: ["pong"],
                                actions: [action_raise!(topic: "ping")])],
                on_entry: [action_log!(message: "pong!")],
            }},
            final_state!{ end {
                result: Output::ValueOf(ValueOfBuilder::default().key("i").build().unwrap()),
            }},
        ]}};
    let ctx = Context::new(sc);
    let mut it = Interpreter::new();
    let result = it.run(&ctx);
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::Int(10));
}
