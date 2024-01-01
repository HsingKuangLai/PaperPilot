import pandas as pd
import math

# 讀取原始CSV文件
file_path = 'All_EachPage_Results.csv'  # 更改為你的檔案路徑
data = pd.read_csv(file_path)

num_chunks = 8
total_rows = len(data)
rows_per_chunk = math.ceil(total_rows / num_chunks)

for i in range(num_chunks):
    start_idx = i * rows_per_chunk
    end_idx = min((i + 1) * rows_per_chunk, total_rows)
    
    chunk_data = data.iloc[start_idx:end_idx]
    chunk_file_path = f'chunk_{i + 1}.csv'
    
    chunk_data.to_csv(chunk_file_path, index=False)
    print(f'Chunk {i + 1} saved to {chunk_file_path}')
