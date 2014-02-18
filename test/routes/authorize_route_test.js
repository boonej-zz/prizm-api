/**
 * Authorization Route Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose')
  , _request      = require('request')
  , _chai         = require('chai')
  , _expect       = _chai.expect
  , _should       = _chai.should()
  , _assert       = _chai.assert
  , _thisapp      = require(process.cwd() + '/server')
  , _auth_route   = require(process.env.PRISM_HOME + 'routes/oauth2/auths')
  , Client        = require(process.env.PRISM_HOME + 'models/auth').ClientApplication
  , Code          = require(process.env.PRISM_HOME + 'models/auth').Code;

describe('Authorization Route Unit Tests', function(done){
  var testClient = null;
  beforeEach(function(done){
    //create test client
    var test = new Client({
      name: 'AuthTest',
      description: 'Here is another desc',
      redirect_uri: 'https://localhost:3000/callback'
    }).save(function(error, client){
      if(error) throw error;
      testClient = client;
      done()
    })
  })
  afterEach(function(done){
    Client.remove({}, function(err){
      if(err) throw err;
    });
    Code.remove({}, function(err){
      if(err) throw err;
      done();
    })
  })
  describe('Testing Fetching a Prism Authorization Code /oauth2/authorize', function(done){
    it('should return a authorization code', function(done){
      var fetch_url = 'https://localhost:3000/oauth2/authorize?client_id=' 
                      + testClient.client_id + '&response_type=code&'
                      + 'redirect_uri=' + testClient.redirect_uri;
      _request({url: fetch_url, json:true, strictSSL: false}, function(err, res, body){
        var result_string = JSON.stringify(body);
        var result = JSON.parse(result_string);
        result = result.data[0];
        result.should.have.property('authorization_code');
        Code.findOne({client_id: testClient.client_id}, function(error, codeResult){
          if(error) throw error;
          _expect(codeResult.code).to.equal(result.authorization_code);
          done();
        })
      });
    });
  });
});
