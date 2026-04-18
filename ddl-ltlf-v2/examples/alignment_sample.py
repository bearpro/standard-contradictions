import mdl


def main() -> None:
    with mdl.ModuleBuilder() as left:
        left.status = mdl.Proposition("status")
        left.require_status = mdl.Rule("left-requires-status", "O", None, left.status)

    with mdl.ModuleBuilder() as right:
        right.status = mdl.Proposition("status")
        right.forbid_status = mdl.Rule("right-forbids-status", "F", None, right.status)

    left_doc = left.build()
    right_doc = right.build()

    matches = mdl.align(left_doc, right_doc)
    assert matches == (mdl.Alignment("status", "status", 1.0),)

    result = mdl.solve(left_doc, right_doc)
    assert not result.is_consistent


if __name__ == "__main__":
    main()
