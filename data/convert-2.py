import json

for i in range(1, 7):
    with open("hsk-"+str(i)+"-update.json", "w", encoding="UTF8") as out_file:
        myjson = json.load(open("hsk-"+str(i)+".json", encoding="UTF8"))
        for word in myjson['words']:
            try:
                del word['learned']
                word["metadata"] = {
                    "learned": False,
                    "description": ""
                }
            except KeyError as exc:
                print('did not have that key')
        out_file.write(
            json.dumps(
                myjson,
                indent=4,
                ensure_ascii=False,
                separators=(',', ': ')))
