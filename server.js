/**********************************************************
 ****             Module Dependencies                  ****
 **********************************************************/
var express   = require('express')
  , mongoose  = require('mongoose')
  , http      = require('http')
  , fs        = require('fs')
  , https     = require('https')
  , passport  = require('passport'),
  , app       = express();

//Express App settings
app.set('views', __dirname + '/views');
app.set('view engine', 'jade');
app.use(express.favicon());
app.use(express.bodyParser());
app.use(express.methodOverride());
app.use(express.router);

//TODO: env dependancy logic here.

//Set SSL options for HTTPS traffic
var ssl_options = {
	key: 				fs.readFileSync(process.env.PRISM_HOME + '/config/ssl/PrismApiDev.key'),
	cert: 				fs.readFileSync(process.env.PRISM_HOME + '/config/ssl/PrismApiDev.crt'),
	ca: 				fs.readFileSync(process.env.PRISM_HOME + '/config/ssl/stablekernel.crt'),
	requestCert: 		true,
	rejectUnauthorized: false
};

//API Routes
app.get('/', function(req,res){ res.send('Welcome to the Prism API') });
/**
app.get('/oauth2/auths', -- create & return code -- );
app.post('/oauth2/tokens' -- create & return tokens -- ); 
*/

//create regular http server listening on port 8080 that re-routes back to https
http.createServer().listen(8080).get('*', function(req, res){
  res.redirect('https://'+ DEVELOPMENT_SERVER_DNS + '/');
});
//create SSL Server listening on port 3000
https.createServer(app).listen(3000, function(){
  console.log("Secure Prism Api server listening on port 3000");
});
