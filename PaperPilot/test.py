import fitz  # PyMuPDF

def extract_text_from_identity_h_pdf(file_path):
    try:
        pdf_document = fitz.open(file_path)
        pdf_text = ''
        
        for page_num in range(pdf_document.page_count):
            page = pdf_document.load_page(page_num)
            text = page.get_text("text")  # Extract text as Unicode
            print(text.encode('utf-8'))
            pdf_text += text
        
        pdf_document.close()
        return pdf_text
    
    except Exception as e:
        print(f"Error: {e}")
        return None

# Usage
file_path = "AR\\106\\1460_宏遠.pdf"
extracted_text = extract_text_from_identity_h_pdf(file_path)
print(extracted_text)