import mdl


def main() -> None:
    with mdl.ModuleBuilder() as left:
        left.person = mdl.ProductType("Person", {"age": mdl.INT, "name": mdl.STRING})
        left.status = mdl.Proposition("status")

    with mdl.ModuleBuilder() as right:
        right.status = mdl.Proposition("status")
        right.decision = mdl.SumType("Decision", {"Ok": mdl.BOOL, "Err": mdl.STRING})

    matches = mdl.align(left.build(), right.build())
    assert matches == (mdl.Alignment("status", "status", 1.0),)


if __name__ == "__main__":
    main()
