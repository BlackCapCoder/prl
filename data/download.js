const fs = require('fs');
const http = require('https');

try
{
  const data = fs.readFileSync('gender.json', 'utf8');
  const json = JSON.parse(data)
  let cnt = 0;
  for (itm of json.results)
  {
    const file = fs.createWriteStream("gender/" + itm.name + ".json");
    const req  = http.get(itm.url, function (response) {
      response.pipe(file);
      file.on('finish', () => {
        file.close();
      });
    });
    cnt++;
  }
} catch (err) {
  console.error(err);
}
