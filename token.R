3edbb07e-d5a2-4968-ade0-b60a42494487





export API_TOKEN=3edbb07e-d5a2-4968-ade0-b60a42494487

export SERVER_URL=https://dataverse.ird.fr/
export PERSISTENT_ID=doi:10.23708/W3TODJ

curl -L -O -J -H "X-Dataverse-key:$API_TOKEN" $SERVER_URL/api/access/dataset/:persistentId/?persistentId=$PERSISTENT_ID

