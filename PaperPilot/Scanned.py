import pdfplumber
import re

def check_scanned_or_native_pdf(pdf_path):
    text = ''
    try:
        with pdfplumber.open(pdf_path) as pdf:
            total_pages = min(len(pdf.pages), 3)  # Limit to reading the first 3 pages or fewer if the PDF has less than 3 pages
            for i in range(total_pages):
                page = pdf.pages[i]
                extracted_text=page.extract_text()
                cleaned_text = re.sub(r'[^\u4e00-\u9fff]', '', extracted_text)
                text += cleaned_text
        # Check if the extracted text is empty or very small (indicating a scanned PDF)
        return not text.strip() or len(text) < 100
    except Exception as e:
        print(f"Error: {e}")
        return False

# Test the function
# result = check_scanned_or_native_pdf("t100sa11_3357_111.pdf")
# print(f"Is the PDF likely scanned?: {result}")