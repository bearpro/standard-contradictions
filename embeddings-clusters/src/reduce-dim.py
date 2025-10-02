import sys
import pandas as pd
import umap 
import os

import umap.get

def main(argv):
    if len(argv) < 2:
        print("Usage: python script.py <input_csv> [<output_csv>]")
        sys.exit(1)

    input_file = argv[1]
    output_file = argv[2] if len(argv) > 2 else "reduced_embeddings.csv"

    # 1. Load csv with embeddings in format "path,x1,x2,...,x1536"
    try:
        df = pd.read_csv(input_file)
    except Exception as e:
        print(f"Ошибка при чтении файла {input_file}: {e}")
        sys.exit(1)
    
    if 'path' not in df.columns:
        print("CSV должен содержать колонку 'path'")
        sys.exit(1)
    
    # Извлекаем эмбеддинги, предполагается, что они начинаются со второй колонки
    embedding_columns = [col for col in df.columns if col != 'path']
    embeddings = df[embedding_columns]
    
    # 2. Reduce embeddings dimensions to 2 using UMAP
    try:
        reducer = umap.UMAP(n_components=2, random_state=42)
        embeddings_2d = reducer.fit_transform(embeddings)
    except Exception as e:
        print(f"Ошибка при снижении размерности: {e}")
        sys.exit(1)
    
    # 3. Save reduced embeddings in format "path,x1,x2"
    df_reduced = pd.DataFrame(embeddings_2d, columns=["x1", "x2"])
    df_reduced.insert(0, "path", df["path"])
    
    try:
        df_reduced.to_csv(output_file, index=False)
        print(f"Reduced embeddings saved to {os.path.abspath(output_file)}")
    except Exception as e:
        print(f"Ошибка при сохранении файла {output_file}: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main(sys.argv)