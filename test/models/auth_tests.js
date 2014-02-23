/**
 * Auth Models Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _request   = require('request')
  , _thisapp   = require(process.cwd() + '/server.js')
  , _mongoose  = require('mongoose')
  , _chai      = require('chai')
  , _expect    = _chai.expect
  , _should    = _chai.should()
  , _assert    = _chai.assert
  , Code       = require(process.env.PRISM_HOME + 'models/auth.js').Code
  , Client     = require(process.env.PRISM_HOME + 'models/auth.js').ClientApplication
  , Token      = require(process.env.PRISM_HOME + 'models/auth.js').Token;

describe('Auth Model Unit Tests', function(done){
  describe('Testing ClientApplication Schema', function(done){
/*    afterEach(function(done){
      Client.remove({}, function(err){
        if(err) throw err;
        done();
      })
    })*/
    describe('Create a Client Application', function(done){
      /**
       * for right now we are not to concerned with defensive unit testing &
       * validation on the client schema. these will be inputed manually as there
       * is only one consumer of the API at this point -- the mobile app.
       */
      it('should successfully create a ClientApplication record', function(done){
        client = new Client({name: "test", description: "test description"})
        client.save(function(error, result){
          _expect(error).to.be.null
          _expect(result).to.be.not.null
          _should.exist(result._id)
          done()
        })
      })
      it('should autogenerate client_id & client_secret on save', function(done){
        client_data = {
          name: 'test client application',
          description: 'description test',
          redirect_uri: 'www.example.com/redirect'
        }
        client = new Client(client_data)
        client.save(function(error, result){
          // console.log(result);
		_expect(error).to.be.null
          _expect(result.client_id).to.have.length.above(10)
          _expect(result.client_secret).to.have.length.above(10)
          result.name.should.equal(client_data.name)
          result.description.should.equal(client_data.description)
          result.redirect_uri.should.equal(client_data.redirect_uri)
          done()
        })
      })
    })
  }),
  describe('Testing Code Schema', function(done){
    afterEach(function(done){
      Code.remove({}, function(err){
        if(err) throw err
        done()
      })
    })
    describe('Create a Code Schema', function(done){
      it('should succesfully create a code application record', function(done){
        code = new Code({client_id: '123235asdgsdf23424'})
        code.save(function(error, result){
          _expect(error).to.be.null
          _expect(result).to.be.not.null
          _should.exist(result._id)
          _expect(result.code).to.have.length.above(5)
          done()
        })
      })
      it('should return an error if the client_id is null', function(done){
        code = new Code({client_id: null})
        code.save(function(error, result){
          _expect(error).to.be.not.null
          done()
        })
      })
      it('should return a valid date_Created attribute when save is inovked', function(done){
        code = new Code({client_id: "asdfasdfasdfhjsdhfjkldf"})
        code.save(function(error, result){
          result.date_created.valueOf().should.be.within(Date.now().valueOf() - 2, Date.now().valueOf() + 2)
          done()
        })
      })
    })
  }),
  describe('Testing Token Schema', function(done){
    var currentClient = null,
        currentCode = null,
        currentToken = null;
    beforeEach(function(done){
      var testClient = new Client({name: "test", description: "description"});
      testClient.save(function(error, client){
        if(error) throw error;
        currentClient = client;
        var testCode = new Code({client_id: client.client_id});
        testCode.save(function(error, code){
          if(error) throw error;
          currentCode = code;
          var testToken = new Token({code: code.code, client_application: client, grant_type: 'authorization_code'})
          .save(function(error, token){
            if(error) throw error;
            currentToken = token;
            done()
          })
        })
      })
    })
    afterEach(function(done){
      Token.remove({}, function(err){
        if(err) throw err;
        done()
      })
    })
    describe('Create a Token Schema', function(done){
      it('should successfully create a token', function(done){
        _should.exist(currentToken._id)
        currentToken.should.have.property('refresh_token')
        currentToken.should.have.property('date_expires')
        currentToken.should.have.property('date_created')
        currentToken.should.have.property('token_type')
        currentToken.should.have.property('code')
        currentToken.should.have.property('client_application')
        currentToken.should.have.property('grant_type')
        _expect(currentToken.access_token).to.have.length.above(10)
        done()
      })
      it('should set the created_date when save is invoked', function(done){
        currentToken.date_created.valueOf().should.be.within(Date.now().valueOf() -2, Date.now().valueOf() + 2)
        done()
      })
      it('should set the date_expires and add an additional 10m', function(done){
        var futureDateToValidate = Date.now() + (10 * 60 * 1000);
        currentToken.date_expires.valueOf().should.be.within(futureDateToValidate.valueOf() -5, futureDateToValidate.valueOf() +5)
        done()
      })
      it('should set the refresh_token when save is invoked', function(done){
        _expect(currentToken.refresh_token).to.have.length.above(10);
        done()
      })
      it('should set the access_token when save is invoked', function(done){
        _expect(currentToken.access_token).to.have.length.above(10);
        done()
      })
      it('should set the token_type code when save is invoked', function(done){
        _expect(currentToken.token_type).to.equal('Bearer')
        done()
      })
      it('should set the client_application _id associated to token when save is invoked', function(done){
        _expect(currentToken.client_application).to.equal(currentClient._id)
        done()
      })
      it('should set the grant_ttype when save is invoked', function(done){
        _expect(currentToken.grant_type).to.equal('authorization_code')
        done()
      })
    })
  })
})
