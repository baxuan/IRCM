def XML_open_file(fileXML,fileCSV):

    import xml.etree.ElementTree as ET

    global file_count
    file_count += 1

    try:
            tree = ET.parse(fileXML)
    except:
        print('error with tree at file number', file_count, fileXML)
        return

    root = tree.getroot()

    Paper_data = open(fileCSV, 'a+', newline='')
    csvwriter = csv.writer(Paper_data)

    for member in root.findall('content'):
        for paper in member.findall('article_rec'):
            paper_content = []
            pdate = ' '
            pid = ' '
            ptitle = ' '
            psubtitle = ' '
            pfulltitle = ' '
            n_o_Authors = 0
            paffiliation = ''
            pconcept = ''

            pdate = paper.find('article_publication_date').text
            paper_content.append(pdate)
            pid = paper.find('article_id').text
            paper_content.append(pid)
            ptitle = paper.find('title').text
            paper_content.append(ptitle)

            pfulltitle = ptitle
            for sub in paper.findall('subtitle'):
                psubtitle = sub.text
                pfulltitle = pfulltitle + ': ' + psubtitle
            paper_content.append(psubtitle)
            paper_content.append(pfulltitle)

            for authors in paper.findall('authors'):
                for au in authors.findall('au'):
                    for aff in au.findall('affiliation'):
                        affiliation=aff.text
                        if n_o_Authors==0:
                            paffiliation = affiliation;
                        else:
                            if paffiliation !="":
                                paffiliation = str(paffiliation) + ' #, ' + str(affiliation);
                            else:
                                paffiliation = str(affiliation);
                    n_o_Authors += 1
            paper_content.append(n_o_Authors)
            paper_content.append(paffiliation)
            for concepts in paper.findall('ccs2012'):
                count = 0
                pconcept=''
                for cc in concepts.findall('concept'):
                    concept = cc.find('concept_desc').text
                    if count==0:
                        pconcept = concept
                    else:
                        pconcept = pconcept + ' #, ' + concept
                    count +=1
            paper_content.append(pconcept)
            csvwriter.writerow(paper_content)
    Paper_data.close()


import csv
import os

working_path = '\\ACM\\'
csv_file = 'Papers.csv'
XML_path1 = '\\ACM\\XML_files\\periodicals\\'
XML_path2 = '\\ACM\\XML_files\\proceedings\\'

os.chdir(working_path)

Paper_data = open(csv_file, 'w', newline='') # open a file for writing
csvwriter = csv.writer(Paper_data) # create the csv writer object
paper_head = ['pdate','paperID','paperTitle', 'paperSubTitle', 'paperFullTitle', 'n_o_Authors', 'Affiliation' , 'paperConcept']
csvwriter.writerow(paper_head)
Paper_data.close()

global file_count
file_count = 0

for root, dirs, files in os.walk(XML_path1):
    for file in files:
        subfolder=os.path.splitext(file)[0]
        XML_open_file(XML_path1+subfolder+'\\'+file,csv_file)

for root, dirs, files in os.walk(XML_path2):
    for file in files:
        subfolder=os.path.splitext(file)[0]
        XML_open_file(XML_path2+subfolder+'\\'+file,csv_file)
