module Pkg.Google () where


-- trackConversion :: Text -> Int -> Text -> Text
-- trackConversion apiKey conversionValue

-- curl -X POST \
--   'https://googleads.googleapis.com/v10/customers/[CUSTOMER_ID]/conversions:upload' \
--   -H "Authorization: Bearer [YOUR_ACCESS_TOKEN]" \
--   -H "Content-Type: application/json" \
--   -d '{
--         "operations": [
--           {
--             "create": {
--               "conversion_action": "customers/[CUSTOMER_ID]/conversionActions/[CONVERSION_ACTION_ID]",
--               "conversion_date_time": "[TIMESTAMP]",
--               "conversion_value": [VALUE],
--               "currency_code": "USD",
--               "gclid": "[GCLID]"
--             }
--           }
--         ]
--       }'
