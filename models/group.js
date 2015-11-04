var mongoose = require('mongoose');
var ObjectId      = mongoose.Schema.Types.ObjectId;
var _ = require('underscore');
var mObjectId = mongoose.Types.ObjectId;

var groupSchema = new mongoose.Schema({
  name          : {type: String, required: true, index: true},
  description   : {type: String, default: ''},
  organization  : {type: ObjectId, ref: 'Organization', required: true},
  leader        : {type: ObjectId, ref: 'User', required: false},
  create_date   : {type: Date},
  status        : {type: String, default: 'active', required: true},
  mutes         : {type: [ObjectId], default: []} 
});

groupSchema.statics.selectFields = function(type){
  return ['_id', 'name', 'description', 'organization', 'leader', 'create_date', 'status', 'mutes'];
};

groupSchema.statics.canResolve = function(){
  return [
    {organization: {identifier: '_id', model: 'Organization'}},
    {leader: {identifier: '_id' , model: 'User'}},
    {mutes: {identifier: '_id', model: 'Mute'}}
  ];
};


groupSchema.methods.format = function(type, add_fields, callback){
  return {
    name        : this.name,
    description : this.description,
    organization: this.organization,
    leader      : this.leader,
    create_date : this.create_date,
    status      : this.status,
    mutes       : this.mutes
  }

};

groupSchema.pre('save', function(next){
  if (!this.create_date) {
    this.create_date = Date.now();
  }
  next();
});

groupSchema.post('init', function(group){
  if (!group.status) {
    group.status = 'active';
  }
});

groupSchema.statics.newGroup = function(obj, next){
  if (obj.organization && obj.name) {
    var model = new this(obj);
    model.save(function(err, group){
      console.log('Error: ' + err);
      console.log('Group: ' + group);
      next(err, group);
    });
  } else {
    next({err: 'no organization or object name'}, false);
  }
}

groupSchema.statics.mute = function(gid, uid, next) {
  var model = this.model('Group');
  model.findOne({_id: gid}, function(err, group) {
    if (group) {
      var exists = false;
      if (!group.mutes) {
        group.mutes = [];
      }
      _.each(group.mutes, function(mute){
        if (String(mute) == String(uid)) {
          exists = true;
        }
      });
      if (!exists) {
        group.mutes.push(uid);
        group.save(function(err, g){
          if (err) console.log(err);
        });
      }
    }
    group = group.toObject();
    group.muted = true;
    next(err, group);
  });
}

groupSchema.statics.unmute = function(gid, uid, next) {
  var model = this.model('Group');
  model.findOne({_id: gid}, function(err, group) {
    if (group) {
      var idx = -1;
      if (!group.mutes) {
        group.mutes = [];
      }
      _.each(group.mutes, function(mute, i){
        if (String(mute) == String(uid)) {
          idx = i;
        }
      });
      if (idx != -1) {
        group.mutes.splice(idx, 1);
        group.save(function(err, g){
          if (err) console.log(err);
        });
      }
    }
    group = group.toObject();
    group.muted = false;

    next(err, group);
  });
};

mongoose.model('Group', groupSchema);
