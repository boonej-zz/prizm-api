/**
 * User Model Unit Tests
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _request   = require('request')
  , _mongoose  = require('mongoose')
  , _chai      = require('chai')
  , _expect    = _chai.expect
  , _should    = _chai.should()
  , _assert    = _chai.assert
  , _server   = require(process.cwd() + '/server')
  , User      = require(process.env.PRISM_HOME + 'models/user.js').User;

describe('User Model Unit Tests', function(done){
  
  afterEach(function(done){
    User.remove({}, function(error){
      if(error) throw error;
      done();
    });
  });
  
  describe('Creating a new User', function(done){
    it('should create a user', function(done){
      var user = new User({
        first_name: 'DJ',
        last_name: 'Hayden',
        email: 'dj.hayden@test.com',
        password: 'passwordtest'
      }).save(function(err, result){
        if(err) throw err;
        _expect(result).to.have.property('_id');
        _expect(result).to.have.property('create_date');
        _expect(result).to.have.property('first_name');
        _expect(result).to.have.property('last_name');
        _expect(result).to.have.property('email');
        _expect(result).to.have.property('password');
        done();
      });
    });
  });
});
