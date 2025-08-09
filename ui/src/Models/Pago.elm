module Models.Pago exposing (..)

import Form.Error as FormError
import Form.Validate as V exposing (Validation)
import Generated.Api exposing (Distribucion, Monto, Pago, ParticipanteId, ShallowPago)
import Utils.Form exposing (CustomFormError(..))



-- TODO: Implement getPagadores


getPagadores : ShallowPago -> List ParticipanteId
getPagadores pago =
    []
