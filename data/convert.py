import yaml
import json

for i in range(1,7):
    with open("hsk-"+str(i)+".yaml", "w", encoding="UTF8") as out_file:
        out_file.write(yaml.dump(json.load(open("hsk-"+str(i)+".json", encoding="UTF8")), allow_unicode=True))
