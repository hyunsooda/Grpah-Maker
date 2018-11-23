const fs = require('fs');
const http = require('http');
const path = require('path');
const express = require('express');
const app = express();
const bodyParser = require('body-parser');
const spawn = require('child_process').spawn;
const buttons = ['인터넷/SNS', '게임', 'IT/과학', 'IT/일반', '컴퓨터', '사회', '경제'];
// setting
app.use(bodyParser.urlencoded({extended: false}));
app.use(bodyParser.json());


function trigger(callback, parameter) {
  const d = new Date();
  const child = spawn('Rscript', ['practice.r', parameter]);
  let result = '';

  child.stdout.on('data', function(data) {
    result += data.toString()
  }); 
    
  child.stderr.on('data', function(data,err) {
    console.log(err)
  });
    
  child.on('exit', function(code, signal) {
    callback.json({
      message: {
        text: result.replace(/"/gi,'')
      },
      keyboard: {
        type: 'buttons',
        buttons
      }
    });
    console.log('success')
    console.log(result)
  });
}
// setInterval(trigger, 2000)


app.get('/keyboard', (req, res) => {
  const data = {
    'type': 'buttons',
    buttons
  }
  res.json(data);
});

app.post('/message', (req, res) => {
  console.log(req.body);
  const msg = req.body.content;
  let num;

  switch(msg) {
    case '게임': num = 1;
    break;
    case '컴퓨터': num = 2;
    break;
    case '인터넷/SNS': num = 3;
    break;
    case 'IT/일반': num = 4;
    break;
    case 'IT/과학': num = 5;
    break;
    case '사회': num = 6;
    break;
    case '경제': num = 7;
    break;
  }
  trigger(res,num);
})



app.listen(7500, function () {
  console.log('Example app listening on port 7500!');
});




/*
http.createServer(function (req, res) {
    fs.readFile('index.html', function (err,data) {
        if (err) {
          res.writeHead(404);
          res.end(JSON.stringify(err));
          return;
        }
        res.writeHead(200);
        res.end(data);
      });
}).listen(8080);
*/