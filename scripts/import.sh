connect to server via alias
ssh -i ~/.ssh/PrismAPIDev.pem ec2-user@ec2-54-186-28-238.us-west-2.compute.amazonaws.com << 'ENDSSH'
mongodump
tar -czvf prism.tar.gz dump/prism
ENDSSH

scp -i ~/.ssh/PrismAPIDev.pem ec2-user@ec2-54-186-28-238.us-west-2.compute.amazonaws.com:/home/ec2-user/prism.tar.gz .
tar xzvf ./prism.tar.gz ./dump/prism


cd ./dump/prism
for file in *.bson
do
  if [[ $file != "system"* ]]
  then
    echo "importing prism collection ${file%.bson}"
    mongorestore --db prism --collection ${file%.bson} --drop $file
  fi
done

echo ""
echo "Finished ...."

