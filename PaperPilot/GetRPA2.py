import csv
import PyPDF2

def full_extract(filepath):
    try:
        pdf_text = ''
        page_list = []

        with open(filepath, 'rb') as pdf_file:
            pdf_reader = PyPDF2.PdfReader(pdf_file)
            total_pages = len(pdf_reader.pages)
            half_pages = total_pages // 2

            for page_num in range(half_pages):
                page = pdf_reader.pages[page_num]
                text = page.extract_text()
                
                if 'RPA' in text: 
                    pdf_text += f"Page {page_num + 1} - {text}\n"
                    page_list.append(page_num + 1)

        pagelist=', '.join(map(str, page_list))
        result=pagelist+'pdfpage'+pdf_text
        return result
    except Exception as e:
        return str(e)


# Input CSV file path
input_csv_file = 'Output.csv'

# Output CSV file path
output_csv_file = 'Output.csv'

# Open and read the input CSV file
with open(input_csv_file, newline='', encoding='utf-8') as input_csv:
    csv_reader = csv.DictReader(input_csv)  # Assuming the first row contains headers
    # print(csv_reader.fieldnames)
    rows = []
    for row in csv_reader:
        # Construct the PDF file path based on columns 'FY', '代號', and '名稱'
        filename = f"AR/{row['FY']}/{row['\ufeff代號']}_{row['名稱']}.pdf"
        
        # Call full_extract function with the constructed file path
        extraction_result = full_extract(filename)
        
        # Split the returned value by 'pdfpage' and assign elements accordingly
        page_text = extraction_result.split('pdfpage', 1)
        row['Page'] = page_text[0].strip() if len(page_text) > 0 else ''
        row['Text'] = page_text[1].strip() if len(page_text) > 1 else ''
        
        rows.append(row)  # Add the updated row to the list of rows

# Write the results to an Output CSV file
with open(output_csv_file, 'w', newline='', encoding='utf-8') as output_csv:
    fieldnames = ['\ufeff代號', '名稱', 'RPA_Count', 'FY', '上傳日期', 'Page', 'Text']  # Adjust field names as needed
    csv_writer = csv.DictWriter(output_csv, fieldnames=fieldnames)
    
    csv_writer.writeheader()  # Write the header row
    csv_writer.writerows(rows)  # Write the rows with updated values
