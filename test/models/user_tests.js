/**
 * User Model Unit Tests
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _request   = require('request'),
    _mongoose  = require('mongoose'),
    _chai      = require('chai'),
    _expect    = _chai.expect,
    _should    = _chai.should(),
    _assert    = _chai.assert,
    _server    = require(process.cwd() + '/server'),
    Trust      = require(process.env.PRISM_HOME + 'models/user.js').Trust,
    User       = require(process.env.PRISM_HOME + 'models/user.js').User;

describe('User Model Unit Tests', function(done){
  var test_user;
  var test_trust_sender;
  var test_trust;
  var test_trust_error;

  var createTrust = function(sender, receiver, cb){
    var t = new Trust({
      user_id: receiver,
      status: 'pending',
      is_owner: true
    }).save(function(err, trust){
      cb(err, trust);
    });
  };

  before(function(done){
      var user = new User({
        first_name: 'DJ',
        last_name: 'Hayden',
        email: 'dj.hayden@test.com',
        password: 'passwordtest'
      }).save(function(err, result){
        if(err) throw err;
        test_user = result;
        var second_user = new User({
          first_name: 'Jack',
          last_name: 'Spade',
          email: 'jack.spade@2coolforschool.com',
          password: 'coolio1'
        }).save(function(err, sec){
          if(err) throw err;
          test_trust_sender = sec;
          createTrust(test_user._id, test_trust_sender._id, function(err, trust){
            test_trust_error = err;
            test_trust = trust;
            done();
          });
        });
      });
  });

  after(function(done){
    User.remove({}, function(error){
      if(error) throw error;
      done();
    });
  });

  describe('Testing Creating a new Trust', function(done){
    it('should successfully create a new trust object', function(done){
      _expect(test_trust).to.have.property('user_id');
      _expect(test_trust).to.have.property('status');
      _expect(test_trust).to.have.property('is_owner');
      _expect(test_trust).to.have.property('create_date');
      _expect(test_trust).to.have.property('modify_date');
      _expect(test_trust).to.have.property('delete_date');
      _expect(test_trust).to.have.property('_id');
      test_trust.user_id.should.equal(test_trust_sender._id);
      test_trust.status.should.equal('pending');
      test_trust.is_owner.should.equal(true);
      done();
    });
    it('should set the create & modify dates', function(done){
      var d = Date.now();
      d = d.valueOf();
      _expect(test_trust.create_date.valueOf()).to.be.within(d-5, d+5);
      _expect(test_trust.modify_date.valueOf()).to.be.within(d-5, d+5);
      done();
    });
    it('should have a null value for delete_date', function(done){
      _expect(test_trust.delete_date).to.equal(null);
      done();
    });
    // NEED MORE NEGATIVE TESTS.
  });
  describe('Creating a new User', function(done){
    it('should create a user', function(done){
      var result = test_user;
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
