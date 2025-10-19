from semantic_segmenter import segment_text
from sys import argv
from json import dumps

path = argv[1]
text = open(path, 'r', encoding='utf-8').read()

# Вариант 1: фиксированный порог
res = segment_text(
    text,
    prefer_sbert=True,        # True — если установлен sentence-transformers и модель уже есть локально
    similarity_threshold=0.80, # настраивайте под нужную «крупность» блоков
    min_sent_per_segment=2
)
result = []
for i, seg in enumerate(res.segments, 1):
    result.append(" ".join(seg.sentences))
    
print(dumps(result, ensure_ascii=False, indent=2))
