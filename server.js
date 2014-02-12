/**
 * Prism Mobile REST API Server
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */

var _express        = require('express')
  , _mongoose       = require('mongoose')
  , _http           = require('http')
  , _fs             = require('fs')
  , _https          = require('https')
  , _passport       = require('passport')
  , _app            = _express()
  , _httpserver     = _express()
  , _prism_auth     = require(process.env.PRISM_HOME + 'routes/oauth2/auths.js')
  , _prism_token    = require(process.env.PRISM_HOME + 'routes/oauth2/tokens.js');

/* environment specific settings */
var env = _app.get('env');
if( env  == 'development' || env == 'local'){
  _app.use(_express.errorHandler());
  _app.use(_express.logger('dev'));
  _mongoose.connect('mongodb://localhost/prism');
}else if( env == 'test' ){
  _app.set('port', 7342);
  _mongoose.connect('mongodb://localhost/prism_test');
}else{
  _mongoose.connect('mongodb://localhost/prism');
}

//general settings */
_app.use(_express.bodyParser());
_app.use(_express.methodOverride());
_app.use(_app.router);

//Set SSL options for HTTPS traffic
var ssl_options = {
	key: 				        _fs.readFileSync(process.env.PRISM_HOME + '/config/ssl/PrismApiDev.key'),
	cert: 			        _fs.readFileSync(process.env.PRISM_HOME + '/config/ssl/PrismApiDev.crt'),
	ca: 				        _fs.readFileSync(process.env.PRISM_HOME + '/config/ssl/stablekernel.crt'),
	requestCert: 		    true,
	rejectUnauthorized: false
};

/* Force all http traffic to https */
_httpserver.set('port', process.env.PORT || 80);
_httpserver.get("*", function(req, res, next){
 // res.redirect("https://" + req.headers.host + ":3000/" + req.path);
  res.redirect("https://localhost:3000" + req.path);
});

/********************** API ROUTES ************************/
/* Root Endpoint */
_app.get('/', function(req,res){ res.send('Welcome to the Prism API') });

/*************** Prism Auth Endpoints *********************/
/* Authentication Code Endpoint */
_app.get('/oauth2/authorize', _prism_auth);
/* Default Authorization Code RedirectUri Callback Endpoint - FOR PRISM MOBILE USE ONLY */
_app.get('/callback', function(req, res){res.send({authorization_code: req.query.code});});
/* Token Request Endpoint */
_app.post('/oauth2/token', _prism_token);

/* create regular http server listening on port 8080 that re-routes back to https */
_http.createServer(_httpserver).listen(8080, function(){
  console.log('HTTP Server listening on port ' + _httpserver.get('port'));
});

/* create SSL Server listening on port 3000 */
_https.createServer(ssl_options, _app).listen(3000, function(){
  console.log("Secure Prism Api server listening on port " + _app.get('port'));
});
