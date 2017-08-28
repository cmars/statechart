#[cfg(test)]

use std::rc::Rc;

extern crate env_logger;
extern crate statechart;
use statechart::*;

#[test]
fn pingpong() {
    let _ = env_logger::init();
    let sc = states!{ root {
        transitions: [goto!(target: ping, actions: [
                action_assign!(key: "i", value: Value::Int(0)),
                action_raise!(topic: "ping", contents: Value::Int(1))])],
        substates: [
            state!{ ping {
                transitions: [
                    goto!(target: pong, topics: ["ping"],
                        cond: cond_fn!(Rc::new(|ctx: &Context| {
                            match ctx.get_var("i") {
                                Some(&Value::Int(i)) => i < 10,
                                _ => false,
                            }})),
                        actions: [
                            action_raise!(topic: "pong"),
                            action_fn!(Rc::new(|ctx: &mut Context| {
                                let i = match ctx.get_var("i") {
                                    Some(&Value::Int(i)) => i,
                                    _ => return Err(Fault::ActionError(
                                        "i: invalid or uninitialized value".to_string())),
                                };
                                ctx.set_var("i", Value::Int(i+1));
                                Ok(())
                            }))]),
                    goto!(target: end,
                        cond: cond_fn!(Rc::new(|ctx: &Context| {
                            match ctx.get_var("i") {
                                Some(&Value::Int(i)) => i >= 10,
                                _ => false,
                            }
                        }))),
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
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::Int(10));
}
