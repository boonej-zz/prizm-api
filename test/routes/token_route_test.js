/**
 * Authorization Token Route Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose')
  , _request      = require('request')
  , _chai         = require('chai')
  , _expect       = _chai.expect
  , _should       = _chai.should()
  , _assert       = _chai.assert
  , _prism_home   = process.env.PRISM_HOME
  , _thisapp      = require(_prism_home + 'server')
  , _auth_model   = require(_prism_home + 'models/auth')
  , Client        = _auth_model.ClientApplication
  , Token         = _auth_model.Token
  , Code          = _auth_model.Code
  , _token_route  = require(_prism_home + 'routes/oauth2/tokens');

describe('Authorization Token Route Unit Tests', function(done){
  var testClient = null;
  var testCode = null;
  
  beforeEach(function(done){
    var test = new Client({
      name: 'AuthTokenTest',
      description: 'Test Description',
      redirect_uri: 'https://localhost:3000/callback'
    }).save(function(error, client){
      if(error) throw error;
      testClient = client;
      var newCode = new Code({
        client_id: client.client_id,
        redirect_uri: client.redirect_uri
      }).save(function(error, code){
        if(error) throw error;
        testCode = code;
        done();
      });
    });
  });
  
  afterEach(function(done){
    Client.remove({}, function(err){
      if(err) throw err;
    });
    Code.remove({}, function(err){
      if(err) throw err;
    });
    Token.remove({}, function(err){
      if(err) throw err;
      done();
    });
  });
  
  describe('Testing Fetching a Prism Authorization Token /oauth2/token', function(done){
    it('should return a valid authorization token with valid client creds', function(done){
      var auth_header = "Basic " + (new Buffer(testClient.client_id + ':' 
                                           + testClient.client_secret).toString('base64'));
                                           console.log(auth_header);
      var request_body = {code: testCode.code, 
                          redirect_uri: testClient.redirect_uri,
                          grant_type: 'authorization_code'};
      var request_url = 'https://localhost:3000/oauth2/token';
      _request({
        method: 'POST',
        url: request_url,
        json: true,
        strictSSL: false,
        body: request_body,
        headers: {'Authorization' : auth_header}
      }, function(error, result, body){
        var string = JSON.stringify(body);
        var token_result = JSON.parse(string);
        token_result.should.have.property('access_token');
        token_result.should.have.property('refresh_token');
        token_result.should.have.property('expires_in');
        token_result.should.have.property('refresh_token');
        _expect(token_result.access_token).to.have.length.above(20);
        _expect(token_result.refresh_token).to.have.length.above(20);
        _expect(token_result.expires_in).to.be.above(100);
        _expect(token_result.token_type).to.equal('Bearer');
        done();
      });
    });
  }); 
});
