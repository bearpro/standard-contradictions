from mdl.aligner import align_sources


def test_aligner_suggests_same_entity_name():
    a = 'module a\nentity email: string\n'
    b = 'module b\nentity email: string\n'
    report = align_sources([a, b])
    assert report.suggestions
    assert report.suggestions[0].score == 1.0
