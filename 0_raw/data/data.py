import requests
import json
from lxml import etree
import csv

headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36'
}


url = 'http://spokentaiwanmandarin.nccu.edu.tw/corpus-data.html'

response = requests.get(url=url).text
tree= etree.HTML(response)

li_list=tree.xpath('//*[@id="sp-left"]/div/div/div/ul/li')
for index in range(1,len(li_list)):
    name=li_list[index].xpath("a/text()")[0]+".csv"
    f = open("C:/Users/JayLee/Desktop/LING 199/data/"+name, 'a',newline='')
    writer = csv.writer(f)
    print(name+"start")
    url1="http://spokentaiwanmandarin.nccu.edu.tw/"+li_list[index].xpath('a/@href')[0]
    response1 = requests.get(url=url1).text
    tree1= etree.HTML(response1)
    tr_list=tree1.xpath('//*[@id="sp-component"]/div/article/div[2]/table/tbody/tr')
    writer.writerow(("Turn","Speaker","Utterence"))
    for tr in tr_list[1:]:
        td_list=tr.xpath('td')
        row=[]
        for td in td_list:
            row.append(td.xpath('p//text()')[0].replace('\xa0',''))
        writer.writerow(row)
    print(name+"end")
    f.close()