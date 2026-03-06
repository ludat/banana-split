module Site.Handler.Receipt (
  handleReceiptImageParse,
) where

import Protolude

import BananaSplit.Monto
import BananaSplit.Receipts (ParsedReceipt (..), ParsedReceiptItem (..), analyzeReceiptImage)
import BananaSplit.Repartija
import BananaSplit.ULID
import Site.Api (ReceiptImageRequest (..), ReceiptImageResponse (..))
import Site.Types

handleReceiptImageParse :: ReceiptImageRequest -> AppHandler ReceiptImageResponse
handleReceiptImageParse req = do
  config <- asks (.receipts)

  result <- liftIO $ analyzeReceiptImage config req.imageBase64

  case result of
    Left err -> pure $ ReceiptImageError{error = err}
    Right parsedReceipt ->
      pure $
        ReceiptImageSuccess
          { items =
              parsedReceipt.items
                <&> \i ->
                  RepartijaItem
                    { id = nullUlid
                    , nombre = i.nombre
                    , monto = scientificToMonto i.monto
                    , cantidad = i.cantidad
                    }
          }
