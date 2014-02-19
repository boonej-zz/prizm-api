/**
 * Prism Mobile REST API Server
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */

process.env["PRISM_HOME"] = process.cwd() + '/';

var _express        = require('express')
  , _mongoose       = require('mongoose')
  , _http           = require('http')
  , _fs             = require('fs')
  , _https          = require('https')
  , _passport       = require('passport')
  , _app            = _express()
  , _httpserver     = _express()
  , _prism_home     = process.env.PRISM_HOME
  , _prism_auth     = require(_prism_home + 'routes/oauth2/auths')
  , _prism_token    = require(_prism_home + 'routes/oauth2/tokens')
  , _prism_user     = require(_prism_home + 'routes/users')
  , _utils          = require(_prism_home + 'utils')
  , _gateway        = require(_prism_home + 'gateway')
  , _config         = require('config');

  console.log(_prism_home);

/* environment specific settings */
var env = _app.get('env');
if( env  == 'development' || env == 'local'){
  _app.use(_express.errorHandler());
  _app.use(_express.logger('dev'));
}else if(env == 'test'){
  //do not load logger dev
}else{
  _app.use(_express.logger('dev'));
}

/* configure mongo connection */
_mongoose.connect('mongodb://' + _config.mongo.host + '/' + _config.mongo.name);

//general settings */
_app.use(_express.bodyParser());
_app.use(_express.methodOverride());
_app.use(_app.router);
//Set SSL options for HTTPS traffic
var ssl_options = {
	key: 				        _fs.readFileSync(_prism_home + '/config/ssl/PrismApiDev.key'),
	cert: 			        _fs.readFileSync(_prism_home + '/config/ssl/PrismApiDev.crt'),
	ca: 				        _fs.readFileSync(_prism_home + '/config/ssl/stablekernel.crt'),
//	requestCert: 		    true,
	rejectUnauthorized: false
};

/* Force all http traffic to https */
_httpserver.set('port', process.env.PORT || 80);
_httpserver.get("*", function(req, res, next){
  res.redirect("https://127.0.0.1:3000" + req.path);
});
process.on('uncaughtException', function (err) {
    console.log(err);
}); 

/********************** API ROUTES ************************/
/* Root Endpoint */
_app.get('/', function(req,res){ res.send('Welcome to the Prism API') });

/*************** Prism Auth Endpoints *********************/
/* Authentication Code Endpoint */
_app.get('/oauth2/authorize', _prism_auth);

/* Default Authorization Code RedirectUri Callback Endpoint - FOR PRISM MOBILE USE ONLY */
_app.get('/callback', function(req, res){
	var array = [{authorization_code: req.query.code}];
	_utils.prismResponse( res, array, true); 
});

/* Token Request Endpoint */
_app.post('/oauth2/token', _prism_token);

/* User Login/Authentication */
_app.post('/oauth2/login', _gateway, _prism_user.login);

/**************** Prism User Endpoints ********************/
/* Create/Register User Route */
_app.post('/user', _gateway, _prism_user.register);

/* Fetch User */
_app.get('/user/:id', _gateway, _prism_user.fetchUser);

/* Fetch A Specific User Object */
// _app.get('/user/:id', _prism_user);

/* Testing Endpoints only */
if(env  == 'test'){
  _app.get('/testresponseformat/:id', function(req, res){
    var array = [{response: 'response1'}, {response: 'response2'}];
    var dict  = {response: 'reponse1'};
    switch(req.param.id){
      case 0:
        _utils.prismResponse(res, array, true);
        break;
    default:
        _utils.prismResponse(res, array, true);
        break;
    }
  });
}

/* create regular http server listening on port 8080 that re-routes back to https */
_http.createServer(_httpserver).listen(8080, function(){
  console.log('HTTP Server listening on port 8080');
});

/* create SSL Server listening on port 3000 */
_https.createServer(ssl_options, _app).listen(_config.env.port, function(){
  console.log("Secure Prism Api server listening on port " + _config.env.port);
});
