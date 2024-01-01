import csv
import fitz

def extract_specific_page(filepath, target_page):
    try:
        # Open the PDF file
        pdf_document = fitz.open(filepath)
        
        # Check if the target_page is within the total number of pages
        if 0 < target_page <= pdf_document.page_count:
            page = pdf_document.load_page(target_page - 1)  # Adjust for 0-based index
            return page.get_text()
        else:
            return f"Page {target_page} out of range for file: {filepath}"
    except Exception as e:
        return str(e)


# Input CSV file path
input_csv_file = 'chunk_1 - 複製.csv'

# Output CSV file path
output_csv_file = 'chunk_1 - 複製.csv'

# Open and read the input CSV file
with open(input_csv_file, newline='', encoding='utf-8') as input_csv:
    csv_reader = csv.DictReader(input_csv)
    rows = []
    
    for row in csv_reader:
        # Construct the PDF file path based on columns 'FY', '代號', and '名稱'
        filename = f"AR/{row['FY']}/{row['\ufeff代號']}_{row['名稱']}.pdf"
        
        # Assuming 'page' column contains the target page number
        target_page = int(row['Page']) if 'Page' in row else 1  # Default to page 1 if 'page' column not found
        
        # Call extract_specific_page function with the constructed file path and target page
        extracted_text = extract_specific_page(filename, target_page)
        
        # Update the CSV row with extracted page number and text
        row['Page'] = str(target_page)
        row['Text'] = extracted_text
        
        rows.append(row)

# Write the results to an Output CSV file
with open(output_csv_file, 'w', newline='', encoding='utf-8') as output_csv:
    fieldnames = ['\ufeff代號', '名稱', 'FY', 'Page', 'Text']  # Adjust field names as needed
    csv_writer = csv.DictWriter(output_csv, fieldnames=fieldnames)
    
    csv_writer.writeheader()  # Write the header row
    csv_writer.writerows(rows)  # Write the rows with updated values
