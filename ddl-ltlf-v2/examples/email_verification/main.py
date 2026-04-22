from __future__ import annotations

from typing import Protocol, cast

import mdl


class EmailModule(Protocol):
    validate: mdl.Function


def _email_module() -> EmailModule:
    with mdl.ModuleBuilder() as doc:
        doc.email_specification = doc.import_mdl("./email_specification.mdl.py")

    module = doc.build()
    email_specification = module.email_specification
    assert isinstance(email_specification, mdl.Module)
    return cast(EmailModule, cast(object, email_specification))


def check_email(email_module: EmailModule, address: str) -> mdl.SolveResult:
    with mdl.ModuleBuilder() as doc:
        doc.email = mdl.Var(mdl.STRING)
        doc.email_value = mdl.Rule(
            "O",
            None,
            mdl.Eq(doc.email, mdl.String(address)),
        )
        doc.email_is_valid = mdl.Rule(
            "O",
            None,
            mdl.Truth(email_module.validate(doc.email)),
        )

    return mdl.solve(doc.build())


def main() -> None:
    email_module = _email_module()

    valid = check_email(email_module, "test@example.com")
    invalid = check_email(email_module, "test@example.con")

    assert valid.is_consistent
    assert not invalid.is_consistent

    print(f"test@example.com: consistent={valid.is_consistent}")
    print(f"test@example.con: consistent={invalid.is_consistent}")


if __name__ == "__main__":
    main()
