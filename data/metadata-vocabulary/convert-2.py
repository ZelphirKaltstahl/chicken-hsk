import json

for i in range(1, 7):
    with open("hsk-"+str(i)+"-including-metadata-update.json", "w", encoding="UTF8") as out_file:
        myjson = json.load(open("hsk-"+str(i)+"-including-metadata.json", encoding="UTF8"))
        for word in myjson['words']:
            try:
                word['translation-data'] = {
                    'english': word['english'],
                    'pinyin-numbered': word['pinyin-numbered'],
                    'pinyin': word['pinyin'],
                    'simplified': word['simplified'],
                    'traditional': word['traditional']
                }
                del word['english']
                del word['pinyin-numbered']
                del word['pinyin']
                del word['simplified']
                del word['traditional']
            except KeyError as exc:
                print('did not have that key')
        out_file.write(
            json.dumps(
                myjson,
                indent=4,
                ensure_ascii=False,
                separators=(',', ': ')))
