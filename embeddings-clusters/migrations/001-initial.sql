CREATE TABLE documents (
    name TEXT PRIMARY KEY
);

CREATE TABLE statements (
    document_name TEXT,
    path TEXT,
    PRIMARY KEY (document_name, path),
    FOREIGN KEY (document_name) REFERENCES documents(name)
);

CREATE TABLE embeddings (
    document_name TEXT,
    statement_path TEXT,
    "index" INTEGER,
    value FLOAT,
    PRIMARY KEY (document_name, statement_path, "index"),
    FOREIGN KEY (document_name) REFERENCES documents(name),
    FOREIGN KEY (document_name, statement_path) REFERENCES statements(document_name, path)
);
