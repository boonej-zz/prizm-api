#!/bin/bash

#### install dependency libs ####
sudo yum -y update
sudo yum -y install nginx
sudo yum -y install gcc-c++ make
sudo yum -y install openssl-devel
sudo yum -y install git

#### setup & install npm & node ####
cd ~/
git clone https://github.com/isaacs/npm.git
git clone git://github.com/joyent/node.git
cd ~/node/
git checkout v0.10.25
./configure
make
sudo make install
cd ~/npm/
sudo make install

#### need to add paths to etc/sudoers secure_path attribute - just do manually for now. ####
# 1. sudo vi /etc.sudoers
# 2. Defaults secure_path = (ADD): :/usr/local/bin

#### setup port forwarding with iptables ####
iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to 8080
iptables -t nat -A PREROUTING -p tcp --dport 443 -j REDIRECT --to 3000

#### install forever ####
sudo npm install -g forever

#### install mongodb & add start on server reboot ####
# create mongodb yum repo file
echo "[10gen]
name=10gen Repository
baseurl=http://downloads-distro.mongodb.org/repo/redhat/os/x86_64
gpgcheck=0" | sudo tee -a /etc/yum.repos.d/10gen-mongodb.repo

# install mongodb via yum
sudo yum -y install mongo-10gen mongo-10gen-server
sudo yum -y install sysstat

# start mongodb on reboot
sudo chkconfig --levels 235 mongod on

# start mongodb
sudo /etc/init.d/mongod start

