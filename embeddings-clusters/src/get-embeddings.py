from sys import argv
from dotenv import load_dotenv
import os
import requests
import csv
import tempfile

load_dotenv()

API_URL = "https://api.openai.com/v1/embeddings"
MODEL = "text-embedding-ada-002"

def get_embedding(text: str, api_key: str) -> list[float]:
    """
    Получает эмбеддинг для переданной строки с использованием OpenAI API.
    """
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json"
    }
    data = {
        "input": text,
        "model": MODEL,
        "encoding_format": "float"
    }
    
    response = requests.post(API_URL, headers=headers, json=data)
    if response.status_code != 200:
        raise Exception(f"API error {response.status_code}: {response.text}")
    
    response_json = response.json()
    # Предполагаем, что передаётся один input, поэтому берем первый элемент списка data
    embedding = response_json["data"][0]["embedding"]
    return embedding

def main(argv):
    if len(argv) < 2:
        print("Usage: python script.py <input_file>")
        exit(1)
    
    input_file = argv[1]
    
    # Читаем строки из файла (игнорируем пустые строки)
    try:
        with open(input_file, "r", encoding="utf-8") as f:
            texts = [line.strip() for line in f if line.strip()]
    except Exception as e:
        print(f"Ошибка чтения файла {input_file}: {e}")
        exit(1)
    
    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        print("Переменная окружения OPENAI_API_KEY не установлена!")
        exit(1)
    
    # Создаем временный CSV-файл для записи результатов
    temp_csv = tempfile.NamedTemporaryFile(delete=False, mode="w", newline='', suffix=".csv", encoding="utf-8")
    csv_writer = csv.writer(temp_csv)
    
    # Заголовок CSV: path,x0,x1,...,x1535 (1536 компонент эмбеддинга)
    header = ["path"] + [f"x{i}" for i in range(1536)]
    csv_writer.writerow(header)
    
    # Обрабатываем каждую строку из входного файла
    for text in texts:
        try:
            embedding = get_embedding(text, api_key)
            if len(embedding) != 1536:
                print(f"Предупреждение: для строки '{text}' эмбеддинг имеет длину {len(embedding)} (ожидалось 1536)")
            # Приводим элементы эмбеддинга к строке для записи в CSV
            row = [text] + [str(x) for x in embedding]
            csv_writer.writerow(row)
        except Exception as e:
            print(f"Ошибка при обработке '{text}': {e}")
    
    temp_csv.close()
    print(f"CSV с результатами сохранен в: {temp_csv.name}")

if __name__ == "__main__":
    main(argv)
