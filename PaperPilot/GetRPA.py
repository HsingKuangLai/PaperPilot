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

# print(full_extract("AR\\106\\3552_同致.pdf"))