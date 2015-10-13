var mongoose          = require('mongoose'),
    utils             = require(process.env.PRISM_HOME + 'utils'),
    User              = mongoose.model('User'),
    ObjectId          = mongoose.Schema.Types.ObjectId;
var mObjectId         = mongoose.Types.ObjectId;
var _ = require('underscore');

var organizationSchema = new mongoose.Schema({
  code                : {type: String, default: null, required: true, 
                          index: true},
  theme               : {type: ObjectId, ref: 'Theme', required: false},
  name                : {type: String, default: null, required: true},
  create_date         : {type: Date, default: null, required: false},
  modify_date         : {type: Date, default: null, required: false},
  members             : {type: Array, default: []},
  welcome_image_url   : {type: String, default: null},
  logo_url            : {type: String, default: null},
  owner               : {type: ObjectId, ref: 'User', required: false},
  namespace           : {type: String, required: false},
  stripe_id           : {type: String, default: null},
  groups              : {type: Array, default: []},
  mutes               : {type: Array, default: []}
});

organizationSchema.pre('save', function(next){
  if (!this.create_date) {
    this.create_date = Date.now();
  }
  this.modify_date = Date.now();
  next();
});

organizationSchema.statics.selectFields = function(type){
  var select = ['id', 'code', 'theme', 'name', 'create_date', 'modify_date',
      'logo_url', 'welcome_image_url', 'groups', 'mutes', 'owner'];
  return select;
};

organizationSchema.methods.format = function(type, add_fields){
  format = {
    _id:          this._id,
    code:         this.code,
    create_date:  this.create_date,
    modify_date:  this.modify_date,
    name:         this.name,
    theme:        this.theme,
    welcome_image_url: this.welcome_image_url,
    logo_url        : this.logo_url,
    groups          : this.groups,
    mutes           : this.mutes,
    owner           : this.owner
  };
  return format;
}

organizationSchema.methods.fetchGroups = function(next) {
  var model = this.model('Organization');
  model.populate(this, {path: 'groups', model: 'Group'}, function(err, organization){
    model.populate(organization, {path:'groups.leader', model: 'User'}, function(err, organization){
      var groups = false;
      var result = [];
      if (organization.groups) {
        groups = _.filter(organization.groups, function(obj) {
          return obj.status == 'active';
        });
        _.each(groups, function(g) {
          g = g.toObject();
          if (g.leader) {
            g.leader_name = g.leader.name;
          }
          result.push(g);
        });
      }
      next(err, result); 
    });
  });
};
organizationSchema.statics.canResolve = function(){
  return [
    {members: {identifier: '_id', model: 'User'}},
    {theme: {identifier: 'code', model: 'Theme'}},
    {groups: {identifier: '_id', model: 'Group'}},
    {mutes: {identifier: '_id', model: 'Mute'}},
    {owner: {identifier: '_id', model: 'User'}}
  ];
}

organizationSchema.statics.findOneAndFlatten = function(oid, next) {
  var model = this.model('Organization');
  var User = this.model('User');
  model.findOne({_id: oid}).
    exec(function(err, organization){
      User.count(
        {
          active: true,
          org_status: {
            $elemMatch: {
              organization: organization._id,
              status: 'active'
            }
          }
        }, function(err, count){
          var result = organization.toObject();
          result.member_count = count;
          next(err, result);
        });
    });
}

var themeSchema = new mongoose.Schema({
  background_url      : {type: String, default: null},
  dominant_color      : {type: String, default: null},
  text_color          : {type: String, default: null},
  create_date         : {type: Date, default: null},
  modify_date         : {type: Date, default: null},
  organization        : {type: ObjectId, ref: 'Organization', required: true}
});

themeSchema.pre('save', function(next){
  if (!this.create_date) {
    this.create_date = Date.now();
  }
  this.modify_date = Date.now();
  next();
});

themeSchema.statics.selectFields = function(type){
  return ['_id', 'background_url', 'dominant_color', 'text_color', 
         'create_date', 'modify_date', 'organization'];
};

themeSchema.methods.format = function(type, add_fields, callback){
  return {
    _id:              this._id,
    background_url:   this.background_url,
    dominant_color:   this.dominant_color,
    text_color:       this.text_color,
    create_date:      this.create_date,
    modify_date:      this.modify_date,
    organization:     this.organization
  };
};

themeSchema.statics.canResolve = function(){
  return [{organization: {identifier: '_id', model: 'Organization'}}];
};


exports.Organization = mongoose.model('Organization', organizationSchema);
exports.Theme = mongoose.model('Theme', themeSchema);
