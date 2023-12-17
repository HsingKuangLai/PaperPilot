import pdfplumber
import re

def read_pdf_text(pdf_path):
    text = ''
    try:
        with pdfplumber.open(pdf_path) as pdf:
            for page in pdf.pages:
                text += page.extract_text()
    except Exception as e:
        print(f"An error occurred: {e}")
    
    return text


# Example usage:
# pdf_file_path = "電腦及週邊設備業_緯創.pdf"  # Replace 'path_to_your_pdf_file.pdf' with the actual file path
# extracted_text = read_pdf_text(pdf_file_path)
# print(extracted_text)  # This will print the extracted text from the PDF file
# print(len(extracted_text))