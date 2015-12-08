var express = require('express')
var app = express()

var TranslationIndex = require('./TranslationIndex.json');
var TranslationMap = require('./TranslationMap.json');

app.get('/translate.json', function (req, res) {
    var key = req.query.s;
    var engkey = TranslationIndex[key];
    var result = TranslationMap[engkey];
    //Inject english version
    result.en = engkey;
    //Return reult as JSON
    res.json(result);
})

var port = 7160
console.log("Starting server on port " + port)
app.listen(7160)