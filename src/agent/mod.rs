use futures::{Async, Poll};
use futures::stream::Stream;
use futures::sync::mpsc;

use ast::Context;
use interpreter::{Event, Fault, Interpreter, Status};

pub struct Agent<'a> {
    pub sender: mpsc::Sender<Event>,
    receiver: mpsc::Receiver<Event>,
    ctx: &'a Context,
    it: Interpreter,
    eos: bool,
}

impl<'a> Stream for Agent<'a> {
    type Item = Status;
    type Error = Fault;
    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        loop {
            let status = self.it.step(self.ctx)?;
            match &status {
                &Status::Done(_) => {
                    if !self.eos {
                        self.eos = true;
                        return Ok(Async::Ready(Some(status)));
                    } else {
                        return Ok(Async::Ready(None));
                    }
                }
                &Status::Blocked => {
                    match self.receiver.poll() {
                        Ok(Async::Ready(Some(event))) => {
                            self.it.push_event(event);
                            continue;
                        }
                        Ok(Async::Ready(None)) => return Err(Fault::BlockedIndefinitely),
                        Ok(Async::NotReady) => return Ok(Async::NotReady),
                        Err(_) => return Err(Fault::BlockedIndefinitely),
                    }
                }
                _ => return Ok(Async::Ready(Some(status))),
            }
        }
    }
}

impl<'a> Agent<'a> {
    pub fn new(ctx: &'a Context, it: Interpreter) -> Agent {
        let (sender, receiver) = mpsc::channel(0);
        Agent {
            sender: sender,
            receiver: receiver,
            ctx: ctx,
            it: it,
            eos: false,
        }
    }
}
