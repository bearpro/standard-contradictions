
import re
from dataclasses import dataclass
from typing import List, Dict, Any, Optional

try:
    from sentence_transformers import SentenceTransformer
    import numpy as np
    _HAS_SBERT = True
except Exception:
    _HAS_SBERT = False
    import numpy as np

try:
    from sklearn.feature_extraction.text import TfidfVectorizer
    from sklearn.preprocessing import normalize as sk_normalize
    _HAS_SKLEARN = True
except Exception:
    _HAS_SKLEARN = False

# ----------------------
# Sentence splitting
# ----------------------

_ABBREV_LIST = {
    "т.д.", "т.п.", "и т.д.", "и т.п.", "г.", "ул.", "рис.", "стр.", "ст.", "пр.", "№",
    "mr.", "mrs.", "ms.", "dr.", "prof.", "inc.", "ltd.", "jr.", "sr.", "vs.", "e.g.", "i.e."
}

def _is_decimal_ending(prev_token: str) -> bool:
    return bool(re.search(r"\d+[.,]\d+$", prev_token))

def split_sentences(text: str) -> List[str]:
    text = text.strip()
    if not text:
        return []
    buffs = []
    cur = []
    i = 0
    n = len(text)
    enders = set(".!?…")
    quotes = set("\"'”»)]")
    while i < n:
        ch = text[i]
        cur.append(ch)
        is_ender = ch in enders
        j = i + 1
        while is_ender and j < n and text[j] in quotes:
            cur.append(text[j])
            j += 1
        boundary = False
        if is_ender:
            k = j
            while k < n and text[k].isspace():
                k += 1
            if k >= n:
                boundary = True
            else:
                nxt = text[k]
                if nxt in "\"“«(" or re.match(r"[A-ZА-ЯЁ0-9]", nxt):
                    prev_token = "".join(cur).rsplit(None, 1)[-1].lower()
                    if prev_token not in _ABBREV_LIST and not _is_decimal_ending(prev_token):
                        boundary = True
        if boundary:
            buffs.append("".join(cur).strip())
            cur = []
            i = j
            continue
        i += 1
    tail = "".join(cur).strip()
    if tail:
        buffs.append(tail)
    if len(buffs) <= 1:
        crude = re.split(r'(?<=[.!?…])\s+', text)
        buffs = [p.strip() for p in crude if p.strip()]
    return buffs

# ----------------------
# Embedding backends
# ----------------------

class EmbeddingBackend:
    def fit(self, sentences: List[str]) -> None: ...
    def encode(self, sentences: List[str]): ...

class TfidfBackend(EmbeddingBackend):
    def __init__(self, lowercase: bool = True, max_features: int = 50000):
        if not _HAS_SKLEARN:
            raise RuntimeError("scikit-learn not available; cannot use TF-IDF backend.")
        self.vect = TfidfVectorizer(lowercase=lowercase, max_features=max_features, analyzer="word")
        self._fitted = False

    def fit(self, sentences: List[str]) -> None:
        self.vect.fit(sentences)
        self._fitted = True

    def encode(self, sentences: List[str]):
        if not self._fitted:
            self.fit(sentences)
        X = self.vect.transform(sentences)
        X = sk_normalize(X, norm="l2", copy=False)
        return X

class SbertBackend(EmbeddingBackend):
    def __init__(self, model_name: str = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2", device: Optional[str] = None):
        if not _HAS_SBERT:
            raise RuntimeError("sentence-transformers not available; cannot use SBERT backend.")
        self.model = SentenceTransformer(model_name, device=device)

    def fit(self, sentences: List[str]) -> None:
        pass

    def encode(self, sentences: List[str]):
        emb = self.model.encode(sentences, convert_to_numpy=True, show_progress_bar=False, normalize_embeddings=True)
        return emb

def make_backend(prefer_sbert: bool = True) -> EmbeddingBackend:
    if prefer_sbert and _HAS_SBERT:
        return SbertBackend()
    return TfidfBackend()

# ----------------------
# Segmentation
# ----------------------

from dataclasses import dataclass

@dataclass
class Segment:
    start: int
    end: int
    sentences: List[str]

@dataclass
class FullResult:
    segments: List["Segment"]
    params: Dict[str, Any]

import scipy.sparse as sp

def _is_sparse(x):
    try:
        return sp.issparse(x)
    except Exception:
        return False

def _centroid(vectors) -> np.ndarray:
    if hasattr(vectors, "toarray"):
        dense = vectors.toarray()
        c = dense.mean(axis=0, keepdims=True)
    else:
        c = vectors.mean(axis=0, keepdims=True)
    norm = np.linalg.norm(c)
    if norm > 0:
        c = c / (norm + 1e-12)
    return c

def _cosine_sim_matrix(A, B) -> np.ndarray:
    if hasattr(A, "toarray"):
        A = A.toarray()
    if hasattr(B, "toarray"):
        B = B.toarray()
    A = A / (np.linalg.norm(A, axis=1, keepdims=True) + 1e-12)
    B = B / (np.linalg.norm(B, axis=1, keepdims=True) + 1e-12)
    return A @ B.T

def greedy_segment(
    sentences: List[str],
    embeddings,
    similarity_threshold: float = 0.78,
    min_sent_per_segment: int = 2,
    max_sent_per_segment: Optional[int] = None
) -> List[Segment]:
    n = len(sentences)
    if n == 0:
        return []
    segs: List[Segment] = []
    s = 0
    while s < n:
        e = s
        cur_vecs = embeddings[s:s+1]
        cur_centroid = _centroid(cur_vecs)
        while e + 1 < n:
            next_vec = embeddings[e+1:e+2]
            sim = float(_cosine_sim_matrix(next_vec, cur_centroid)[0, 0])
            if max_sent_per_segment is not None and (e - s + 1) >= max_sent_per_segment:
                break
            if sim >= similarity_threshold or (e - s + 1) < min_sent_per_segment:
                e += 1
                # stack depending on sparsity
                nxt = embeddings[e:e+1]
                if _is_sparse(cur_vecs) or _is_sparse(nxt):
                    cur_vecs = sp.vstack([cur_vecs, nxt])
                else:
                    import numpy as _np
                    cur_vecs = _np.vstack([cur_vecs, nxt])
                cur_centroid = _centroid(cur_vecs)
            else:
                break
        segs.append(Segment(start=s, end=e, sentences=sentences[s:e+1]))
        s = e + 1
    return segs

def simulate_avg_len(
    sentences: List[str],
    embeddings,
    threshold: float,
    min_sent_per_segment: int = 2,
    max_sent_per_segment: Optional[int] = None
) -> float:
    segs = greedy_segment(
        sentences, embeddings,
        similarity_threshold=threshold,
        min_sent_per_segment=min_sent_per_segment,
        max_sent_per_segment=max_sent_per_segment
    )
    lens = [len(seg.sentences) for seg in segs] or [0]
    return sum(lens) / max(len(lens), 1)

def auto_calibrate_threshold(
    sentences: List[str],
    embeddings,
    target_avg_len: int,
    min_sent_per_segment: int = 2,
    max_sent_per_segment: Optional[int] = None,
    low: float = 0.5,
    high: float = 0.92,
    tol: float = 0.25,
    max_iters: int = 12
) -> float:
    lo, hi = low, high
    best_thr = hi
    best_err = float("inf")
    for _ in range(max_iters):
        mid = (lo + hi) / 2.0
        avg_len = simulate_avg_len(sentences, embeddings, mid, min_sent_per_segment, max_sent_per_segment)
        err = abs(avg_len - target_avg_len)
        if err < best_err:
            best_err, best_thr = err, mid
        if avg_len > target_avg_len:
            lo = mid
        else:
            hi = mid
        if err <= tol:
            break
    return float(best_thr)

def segment_text(
    text: str,
    prefer_sbert: bool = True,
    similarity_threshold: Optional[float] = 0.78,
    target_avg_len: Optional[int] = None,
    min_sent_per_segment: int = 2,
    max_sent_per_segment: Optional[int] = None
) -> FullResult:
    sents = split_sentences(text)
    backend = make_backend(prefer_sbert=prefer_sbert)
    emb = backend.encode(sents)

    thr = similarity_threshold
    if thr is None and target_avg_len is None:
        thr = 0.78
    if target_avg_len is not None:
        thr = auto_calibrate_threshold(
            sents, emb, target_avg_len=target_avg_len,
            min_sent_per_segment=min_sent_per_segment,
            max_sent_per_segment=max_sent_per_segment
        )
    segments = greedy_segment(
        sents, emb,
        similarity_threshold=thr,
        min_sent_per_segment=min_sent_per_segment,
        max_sent_per_segment=max_sent_per_segment
    )
    return FullResult(
        segments=segments,
        params=dict(
            similarity_threshold=float(thr),
            min_sent_per_segment=min_sent_per_segment,
            max_sent_per_segment=max_sent_per_segment,
            prefer_sbert=prefer_sbert
        )
    )
