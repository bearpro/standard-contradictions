EMAIL_SOURCE = """
@ rfc2822
module email

import "std/system/strings.mdl" as strings
import "std/collections/list.mdl" as List exposing (List)

private type ProcessingState = LocalPart(unit) | Domain(unit)

private func process_email(
    email: List<string>,
    state: ProcessingState
) -> bool:
    case email:
    | List.Cons(char, tail):
        if state = LocalPart(()) and char = "@"
        then process_email(tail, Domain(()))
        else if char = "@" then false else process_email(tail, state)
    | List.Empty(()):
        true

private func email_is_correct(email: string) -> bool:
    let char_list = strings.to_list(email)
    let state = LocalPart(())
    process_email(char_list, state)

entity email: string

event email_received(email: string)

@ 3.4.1. Addr-spec specification
rule O email_addr_spec_correct: email_is_correct(email) always

rule F malformed_email_received: (email_received(email) and not email_is_correct(email)) eventually

align email to <urn:ietf:rfc:2822:addr-spec> equivalent

fact email = "ti@example.org"

assert email_is_correct(email) eventually
"""

PIPE_SOURCE = """
module pipe_spec

type Pipe = { length: rat, radius: rat }

entity pipe: Pipe

rule O pipe_length_positive: pipe.length > 0 always

fact pipe = Pipe { length = 10, radius = 2 }
"""

TUBE_SOURCE = """
module tube

type Tube = { length: rat, r: rat }

entity tube: Tube

rule O r1: tube.length > 0 always
"""

ALIGNMENT_SOURCE = """
module alignment_pipe_spec_tube

import "./pipe.mdl" as m1
import "./tube.mdl" as m2

# align kind=field score=0.583 matcher=bdikit:coma
rule O alignment_001: (m1.pipe.length = m2.tube.length) always

# align kind=field score=0.266 matcher=bdikit:coma
rule O alignment_002: (m1.pipe.radius = m2.tube.r) always

# align kind=entity score=0.100 matcher=bdikit:coma
rule O alignment_003: (m1.pipe = m2.tube) always
"""

FIB_SOURCE = """
module fib

func fib(n: int) -> int:
    if n <= 2 then 1
    else fib(n - 1) + fib(n - 2)

entity fibonacci_number: int
entity fibonacci_number_index: int

rule O r1: (fibonacci_number = fib(fibonacci_number_index)) always

fact fibonacci_number > 200
"""

PWR_SOURCE = """
module pwr

func pwr(a: int, n: int) -> int:
    if n = 0 then 1
    else a * pwr(a, n - 1)

entity number: int
entity power: int
entity result: int

rule O power_positive: power >= 0 always
rule O max_power: power < 20 always # constrain max power to limit recursion depth
rule O result_is_power: result = pwr(number, power) always
"""

LINEQ_RAT_SOURCE = """
module lineq_rat

import "std/collections/list.mdl" as List exposing (List, len)


type Term = {
    coef: rat,
    var: rat
}

func pwr(a: rat, n: int) -> rat:
    if n = 0 then 1
    else a * pwr(a, n - 1)

func value(equation: List<Term>) -> rat:
    case equation:
    | List.Empty(()): 0
    | List.Cons(head, e): 
        let pow = 1 + len(equation)
        let termValue = (head).coef * pwr((head).var, pow)
        termValue + value(e)

# Helps solver to not iterate over int32^pow
func values_lt_3(equation: List<Term>) -> bool:
    case equation:
    | List.Empty(()): true
    | List.Cons(head, tail): 
        (head).coef < 3 and (head).var < 3 and values_lt_3(tail)

entity equation: List<Term>

rule O r0: value(equation) = 1 always
rule O r1: len(equation) = 2 always
# rule O r2: values_lt_3(equation) always

fact case equation:
     | List.Cons(a, List.Cons(b, List.Empty(()))):
        ((a).coef = 2) and ((b).coef = -1)
     | _: false
"""
