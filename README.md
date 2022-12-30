# taiwan-mandarin-corpus-annotation
An annotation for the NCCU Taiwanese Mandarin Corpus, using [Rezonator](https://github.com/johnwdubois/rezonator/projects/1) and based on discourse-functional principles. This is a collaborative project involving students at the University of California, Santa Barbara.

## Workflow

### Tokenisation

1. Step 1: Use 1_auto_split.R to automatically split the text into intonation units; result should be stored in 1_auto_split
2. Step 2: Use 2_auto_tokenise.R to automatically tokenise the text; result should be stored in 2_auto_tokenized
3. Step 3: Copy the file to 3_manual_tokenised and add your name to the end (e.g. NCCU-TM009-CN-FFF_Ryan.csv). Use a program like Pulsar to open the CSV, then listen to the recording and correct the tokenisation. At the same time that you tokenise, listen for errors in the transcription and correct them. This should be done separately by two people. Instructions can be found in the 'Tokenisation ...'  ppt.
4. Step 4: Check with your partner and agree on a final tokenisation + transcription. It would be useful to use a diffchecker https://www.diffchecker.com/) to make sure you catch all the differences. Put the final tokenisation in 4_final_tokenised.

### CSV > Rez conversion
5. Step 5: Use 5_dft_convert.R to automatically convert the FINAL tokenisation to DFT format. Put the result in the 5_dft_converted folder
6. Step 6: Use the file 6_to_rez.R to convert the DFT-formatted file in 5_dft_converted into owpl (one-word-per-line) .csv format in the 6_to_rez folder
7. Step 7: Import the owpl .csv in the 6_to_rez  folder into Rezonator using the owpl_mandarin.json schema file and save the .rez file as e.g. NCCU-TM009-CN-FFF.rez:

![image](https://user-images.githubusercontent.com/43101723/209837860-d085336e-db5a-4434-b44a-75f282b699a1.png)

### IU segmentation
8. Step 8: Copy the .rez file to 8_manual_split and add your name, e.g. NCCU-TM009-CN-FFF-Ryan.rez. Then listen to the folder, correct the correct IU splits and add endnotes ('punctuation'). Instructions can be found in the 'IU splitting ...'  ppt.
9. Step 9: A third person should take the .rez files done by the two people in Step 8, give tie-breaking votes on the differences, and then save the file in the 9_final_split folder.

### Postprocessing
10. Step 10: Use the 10_final_csv.R file to convert the .rez file into two new formats: a .txt file in 10_final_csv_unit + a .csv file in 10_final_csv_owpl folders.
11. Step 11: Import the .csv file into Rezonator and save it in the 11_final_rez folder using the import schema 11_owpl_end.json.
