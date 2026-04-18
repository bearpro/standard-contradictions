import mdl


def main() -> None:
    with mdl.ModuleBuilder() as left:
        left.status = mdl.Proposition()
        left.require_status = mdl.Rule("O", None, left.status)

    with mdl.ModuleBuilder() as right:
        right.status = mdl.Proposition()
        right.forbid_status = mdl.Rule("F", None, right.status)

    left_doc = left.build()
    right_doc = right.build()

    matches = mdl.align(left_doc, right_doc)

    result = mdl.solve(left_doc, right_doc, alignments=matches)
    print(result)


if __name__ == "__main__":
    main()
