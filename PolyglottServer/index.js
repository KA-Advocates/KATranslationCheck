var express = require('express')
var app = express()

var TranslationIndex = require('./TranslationIndex.json');
var TranslationMap = require('./TranslationMap.json');

app.get('/translate.json', function (req, res) {
    var key = req.query.s;
    try {
        var engkey = TranslationIndex[key];
        var result = TranslationMap[engkey];
        //Inject english version
        result.en = engkey;
        res.json(result);
    } catch(err) {
        //Not found -> Empty result
        res.json({})
    }
})

var port = 7160
console.log("Starting server on port " + port)
app.listen(7160)
