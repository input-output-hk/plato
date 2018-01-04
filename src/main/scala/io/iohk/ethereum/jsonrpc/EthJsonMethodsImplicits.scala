package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.InvalidParams
import io.iohk.ethereum.jsonrpc.PersonalService.{SendTransactionRequest, SendTransactionResponse, SignRequest}
import org.json4s.{Extraction, JsonAST}
import org.json4s.JsonAST.{JArray, JBool, JString, JValue, _}
import org.json4s.JsonDSL._

// scalastyle:off number.of.methods
object EthJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val eth_protocolVersion = new JsonDecoder[ProtocolVersionRequest] with JsonEncoder[ProtocolVersionResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, ProtocolVersionRequest] = Right(ProtocolVersionRequest())

    def encodeJson(t: ProtocolVersionResponse): JValue = t.value
  }

  implicit val eth_blockNumber = new JsonDecoder[BestBlockNumberRequest] with JsonEncoder[BestBlockNumberResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, BestBlockNumberRequest] = Right(BestBlockNumberRequest())

    override def encodeJson(t: BestBlockNumberResponse): JValue = Extraction.decompose(t.bestBlockNumber)
  }

  implicit val eth_gasPrice = new JsonDecoder[GetGasPriceRequest] with JsonEncoder[GetGasPriceResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetGasPriceRequest] = params match {
      case None | Some(JArray(Nil)) => Right(GetGasPriceRequest())
      case Some(_) => Left(InvalidParams())
    }

    override def encodeJson(t: GetGasPriceResponse): JValue =  encodeAsHex(t.price)
  }

  implicit val eth_hashrate = new JsonDecoder[GetHashRateRequest] with JsonEncoder[GetHashRateResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetHashRateRequest] = params match {
      case None | Some(JArray(Nil)) => Right(GetHashRateRequest())
      case Some(_) => Left(InvalidParams())
    }

    override def encodeJson(t: GetHashRateResponse): JsonAST.JValue = encodeAsHex(t.hashRate)
  }

  implicit val eth_mining = new JsonDecoder[GetMiningRequest] with JsonEncoder[GetMiningResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetMiningRequest] = params match {
      case None | Some(JArray(Nil)) => Right(GetMiningRequest())
      case Some(_) => Left(InvalidParams())
    }

    override def encodeJson(t: GetMiningResponse): JValue = JBool(t.isMining)
  }

  implicit val eth_getBlockTransactionCountByHash = new JsonDecoder[TxCountByBlockHashRequest] with JsonEncoder[TxCountByBlockHashResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, TxCountByBlockHashRequest] =
      params match {
        case Some(JArray(JString(input) :: Nil)) =>
          extractHash(input).map(TxCountByBlockHashRequest)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: TxCountByBlockHashResponse): JValue =
      Extraction.decompose(t.txsQuantity.map(BigInt(_)))
  }

  implicit val eth_getBlockByHash = new JsonDecoder[BlockByBlockHashRequest] with JsonEncoder[BlockByBlockHashResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, BlockByBlockHashRequest] = {
      params match {
        case Some(JArray(JString(blockHash) :: JBool(fullTxs) :: Nil)) =>
          extractHash(blockHash).map(BlockByBlockHashRequest(_, fullTxs))
        case _ => Left(InvalidParams())
      }
    }

    override def encodeJson(t: BlockByBlockHashResponse): JValue =
      Extraction.decompose(t.blockResponse)
  }

  implicit val eth_getBlockByNumber = new JsonDecoder[BlockByNumberRequest] with JsonEncoder[BlockByNumberResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, BlockByNumberRequest] = {
      params match {
        case Some(JArray(blockStr :: JBool(fullTxs) :: Nil)) =>
          extractBlockParam(blockStr).map(BlockByNumberRequest(_, fullTxs))
        case _ => Left(InvalidParams())
      }
    }

    override def encodeJson(t: BlockByNumberResponse): JValue =
      Extraction.decompose(t.blockResponse)
  }

  implicit val eth_getTransactionByHash =
    new JsonDecoder[GetTransactionByHashRequest] with JsonEncoder[GetTransactionByHashResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionByHashRequest] = params match {
        case Some(JArray(JString(txHash) :: Nil)) =>
          for {
            parsedTxHash <- extractHash(txHash)
          } yield GetTransactionByHashRequest(parsedTxHash)
        case _ => Left(InvalidParams())
      }

      override def encodeJson(t: GetTransactionByHashResponse): JValue =
        Extraction.decompose(t.txResponse)
    }

  implicit val eth_getTransactionReceipt =
    new JsonDecoder[GetTransactionReceiptRequest] with JsonEncoder[GetTransactionReceiptResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionReceiptRequest] = params match {
        case Some(JArray(JString(txHash) :: Nil)) =>
          for {
            parsedTxHash <- extractHash(txHash)
          } yield GetTransactionReceiptRequest(parsedTxHash)
        case _ => Left(InvalidParams())
      }

      override def encodeJson(t: GetTransactionReceiptResponse): JValue =
        Extraction.decompose(t.txResponse)
    }

  implicit val eth_getTransactionByBlockHashAndIndex =
    new JsonDecoder[GetTransactionByBlockHashAndIndexRequest] with JsonEncoder[GetTransactionByBlockHashAndIndexResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionByBlockHashAndIndexRequest] = params match {
        case Some(JArray(JString(blockHash) :: transactionIndex :: Nil)) =>
          for {
            parsedBlockHash <- extractHash(blockHash)
            parsedTransactionIndex <- extractQuantity(transactionIndex)
          } yield GetTransactionByBlockHashAndIndexRequest(parsedBlockHash, parsedTransactionIndex)
        case _ => Left(InvalidParams())
      }

      override def encodeJson(t: GetTransactionByBlockHashAndIndexResponse): JValue =
        t.transactionResponse.map(Extraction.decompose).getOrElse(JNull)
    }

  implicit val eth_getTransactionByBlockNumberAndIndex  =
    new JsonDecoder[GetTransactionByBlockNumberAndIndexRequest] with JsonEncoder[GetTransactionByBlockNumberAndIndexResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionByBlockNumberAndIndexRequest] = params match {
        case Some(JArray(blockParam :: transactionIndex :: Nil)) =>
          for {
            blockParam <- extractBlockParam(blockParam)
            parsedTransactionIndex <- extractQuantity(transactionIndex)
          } yield GetTransactionByBlockNumberAndIndexRequest(blockParam, parsedTransactionIndex)
        case _ => Left(InvalidParams())
      }

      override def encodeJson(t: GetTransactionByBlockNumberAndIndexResponse): JValue =
        t.transactionResponse.map(Extraction.decompose).getOrElse(JNull)
    }

  implicit val eth_getUncleByBlockHashAndIndex = new JsonDecoder[UncleByBlockHashAndIndexRequest] with JsonEncoder[UncleByBlockHashAndIndexResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, UncleByBlockHashAndIndexRequest] =
      params match {
        case Some(JArray(JString(blockHash) :: uncleIndex :: Nil)) =>
          for {
            hash <- extractHash(blockHash)
            uncleBlockIndex <- extractQuantity(uncleIndex)
          } yield UncleByBlockHashAndIndexRequest(hash, uncleBlockIndex)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: UncleByBlockHashAndIndexResponse): JValue = {
      val uncleBlockResponse = Extraction.decompose(t.uncleBlockResponse)
      uncleBlockResponse.removeField{
        case JField("transactions", _) => true
        case _ => false
      }
    }
  }

  implicit val eth_getUncleByBlockNumberAndIndex = new JsonDecoder[UncleByBlockNumberAndIndexRequest] with JsonEncoder[UncleByBlockNumberAndIndexResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, UncleByBlockNumberAndIndexRequest] =
      params match {
        case Some(JArray(blockStr :: uncleIndex :: Nil)) =>
          for {
            block <- extractBlockParam(blockStr)
            uncleBlockIndex <- extractQuantity(uncleIndex)
          } yield UncleByBlockNumberAndIndexRequest(block, uncleBlockIndex)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: UncleByBlockNumberAndIndexResponse): JValue = {
      val uncleBlockResponse = Extraction.decompose(t.uncleBlockResponse)
      uncleBlockResponse.removeField{
        case JField("transactions", _) => true
        case _ => false
      }
    }
  }

  implicit val eth_syncing = new JsonDecoder[SyncingRequest] with JsonEncoder[SyncingResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, SyncingRequest] = Right(SyncingRequest())

    def encodeJson(t: SyncingResponse): JValue = t.syncStatus match {
      case Some(syncStatus) => Extraction.decompose(syncStatus)
      case None => false
    }
  }

  implicit val eth_sendRawTransaction = new JsonDecoder[SendRawTransactionRequest] with JsonEncoder[SendRawTransactionResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, SendRawTransactionRequest] =
      params match {
        case Some(JArray(JString(dataStr) :: Nil)) =>
          for {
            data <- extractBytes(dataStr)
          } yield SendRawTransactionRequest(data)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: SendRawTransactionResponse): JValue = encodeAsHex(t.transactionHash)
  }

  implicit val eth_sendTransaction = new Codec[SendTransactionRequest, SendTransactionResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, SendTransactionRequest] =
      params match {
        case Some(JArray(JObject(tx) :: _)) =>
          extractTx(tx.toMap).map(SendTransactionRequest)
        case _ =>
          Left(InvalidParams())
      }

    def encodeJson(t: SendTransactionResponse): JValue =
      encodeAsHex(t.txHash)
  }

  implicit val eth_call = new JsonDecoder[CallRequest] with JsonEncoder[CallResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, CallRequest] =
      params match {
        case Some(JArray((txObj: JObject) :: (blockValue: JValue) :: Nil)) =>
          for {
            blockParam <- extractBlockParam(blockValue)
            tx <- extractCall(txObj)
          } yield CallRequest(tx, blockParam)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: CallResponse): JValue = encodeAsHex(t.returnData)
  }

  implicit val eth_estimateGas = new JsonDecoder[CallRequest] with JsonEncoder[EstimateGasResponse] {
    override def encodeJson(t: EstimateGasResponse): JValue = encodeAsHex(t.gas)

    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, CallRequest] =
      withoutBlockParam.applyOrElse(params, eth_call.decodeJson)

    def withoutBlockParam: PartialFunction[Option[JArray], Either[JsonRpcError, CallRequest]] = {
      case Some(JArray((txObj: JObject) :: Nil)) =>
        extractCall(txObj).map(CallRequest(_, BlockParam.Latest))
    }

  }

  implicit val eth_getCode = new JsonDecoder[GetCodeRequest] with JsonEncoder[GetCodeResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetCodeRequest] =
      params match {
        case Some(JArray((address: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            addr <- extractAddress(address)
            block <- extractBlockParam(blockValue)
          } yield GetCodeRequest(addr, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetCodeResponse): JValue = encodeAsHex(t.result)
  }

  implicit val eth_getUncleCountByBlockNumber = new JsonDecoder[GetUncleCountByBlockNumberRequest] with JsonEncoder[GetUncleCountByBlockNumberResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetUncleCountByBlockNumberRequest] =
      params match {
        case Some(JArray((blockValue: JValue) :: Nil)) =>
          for {
            block <- extractBlockParam(blockValue)
          } yield GetUncleCountByBlockNumberRequest(block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetUncleCountByBlockNumberResponse): JValue = encodeAsHex(t.result)
  }

  implicit val eth_getUncleCountByBlockHash = new JsonDecoder[GetUncleCountByBlockHashRequest] with JsonEncoder[GetUncleCountByBlockHashResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetUncleCountByBlockHashRequest] =
      params match {
        case Some(JArray(JString(hash) :: Nil)) =>
          for {
            blockHash <- extractHash(hash)
          } yield GetUncleCountByBlockHashRequest(blockHash)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetUncleCountByBlockHashResponse): JValue = encodeAsHex(t.result)
  }

  implicit val eth_getBlockTransactionCountByNumber = new JsonDecoder[GetBlockTransactionCountByNumberRequest] with
    JsonEncoder[GetBlockTransactionCountByNumberResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetBlockTransactionCountByNumberRequest] =
      params match {
        case Some(JArray((blockValue: JValue) :: Nil)) =>
          for {
            block <- extractBlockParam(blockValue)
          } yield GetBlockTransactionCountByNumberRequest(block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetBlockTransactionCountByNumberResponse): JValue = encodeAsHex(t.result)
  }

  implicit val eth_getBalance = new JsonDecoder[GetBalanceRequest] with JsonEncoder[GetBalanceResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetBalanceRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockValue)
          } yield GetBalanceRequest(address, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetBalanceResponse): JValue = encodeAsHex(t.value)
  }

  implicit val eth_getStorageAt = new JsonDecoder[GetStorageAtRequest] with JsonEncoder[GetStorageAtResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetStorageAtRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (positionStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            position <- extractQuantity(positionStr)
            block <- extractBlockParam(blockValue)
          } yield GetStorageAtRequest(address, position, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetStorageAtResponse): JValue = encodeAsHex(t.value)
  }

  implicit val eth_getTransactionCount = new JsonDecoder[GetTransactionCountRequest] with JsonEncoder[GetTransactionCountResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionCountRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockValue)
          } yield GetTransactionCountRequest(address, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetTransactionCountResponse): JValue = encodeAsHex(t.value)
  }

  implicit val newFilterResponseEnc = new JsonEncoder[NewFilterResponse] {
    def encodeJson(t: NewFilterResponse): JValue = encodeAsHex(t.filterId)
  }

  implicit val eth_newFilter = new JsonDecoder[NewFilterRequest] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, NewFilterRequest] =
      params match {
        case Some(JArray((filterObj: JObject) :: Nil)) =>
          for {
            filter <- extractFilter(filterObj)
          } yield NewFilterRequest(filter)
        case _ => Left(InvalidParams())
      }
  }

  implicit val eth_newBlockFilter = new JsonDecoder[NewBlockFilterRequest] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, NewBlockFilterRequest] =
      Right(NewBlockFilterRequest())
  }

  implicit val eth_newPendingTransactionFilter = new JsonDecoder[NewPendingTransactionFilterRequest] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, NewPendingTransactionFilterRequest] =
      Right(NewPendingTransactionFilterRequest())
  }

  implicit val eth_uninstallFilter = new JsonDecoder[UninstallFilterRequest] with JsonEncoder[UninstallFilterResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, UninstallFilterRequest] =
      params match {
        case Some(JArray((rawFilterId: JValue) :: Nil)) =>
          for {
            filterId <- extractQuantity(rawFilterId)
          } yield UninstallFilterRequest(filterId)
        case _ => Left(InvalidParams())
      }
    override def encodeJson(t: UninstallFilterResponse): JValue = JBool(t.success)
  }

  implicit val eth_getFilterChanges = new JsonDecoder[GetFilterChangesRequest] with JsonEncoder[GetFilterChangesResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetFilterChangesRequest] =
      params match {
        case Some(JArray((rawFilterId: JValue) :: Nil)) =>
          for {
            filterId <- extractQuantity(rawFilterId)
          } yield GetFilterChangesRequest(filterId)
        case _ => Left(InvalidParams())
      }
    override def encodeJson(t: GetFilterChangesResponse): JValue =
      t.filterChanges match {
        case FilterManager.LogFilterChanges(logs) => JArray(logs.map(Extraction.decompose).toList)
        case FilterManager.BlockFilterChanges(blockHashes) =>  JArray(blockHashes.map(encodeAsHex).toList)
        case FilterManager.PendingTransactionFilterChanges(txHashes) => JArray(txHashes.map(encodeAsHex).toList)
      }
  }

  implicit val eth_getFilterLogs = new JsonDecoder[GetFilterLogsRequest] with JsonEncoder[GetFilterLogsResponse] {
    import FilterManager._

    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetFilterLogsRequest] =
      params match {
        case Some(JArray((rawFilterId: JValue) :: Nil)) =>
          for {
            filterId <- extractQuantity(rawFilterId)
          } yield GetFilterLogsRequest(filterId)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: GetFilterLogsResponse): JValue =
      t.filterLogs match {
        case LogFilterLogs(logs) => JArray(logs.map(Extraction.decompose).toList)
        case BlockFilterLogs(blockHashes) =>  JArray(blockHashes.map(encodeAsHex).toList)
        case PendingTransactionFilterLogs(txHashes) => JArray(txHashes.map(encodeAsHex).toList)
      }
  }

  implicit val eth_getLogs = new JsonDecoder[GetLogsRequest] with JsonEncoder[GetLogsResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetLogsRequest] =
      params match {
        case Some(JArray((filterObj: JObject) :: Nil)) =>
          for {
            filter <- extractFilter(filterObj)
          } yield GetLogsRequest(filter)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: GetLogsResponse): JValue =
      JArray(t.filterLogs.logs.map(Extraction.decompose).toList)
  }

  private def extractFilter(obj: JObject): Either[JsonRpcError, Filter] = {
    def allSuccess[T](eithers: Seq[Either[JsonRpcError, T]]): Either[JsonRpcError, Seq[T]] = {
      if (eithers.forall(_.isRight)) Right(eithers.map(_.right.get))
      else Left(InvalidParams(msg = eithers.collect { case Left(err) => err.message }.mkString("\n")))
    }

    def parseTopic(jstr: JString): Either[JsonRpcError, ByteString] = {
      extractBytes(jstr).left.map(_ => InvalidParams(msg = s"Unable to parse topics, expected byte data but got ${jstr.values}"))
    }

    def parseNestedTopics(jarr: JArray): Either[JsonRpcError, Seq[ByteString]] = {
      allSuccess(jarr.arr.map {
        case jstr: JString => parseTopic(jstr)
        case other => Left(InvalidParams(msg = s"Unable to parse topics, expected byte data but got: $other"))
      })
    }

    val topicsEither: Either[JsonRpcError, Seq[Seq[ByteString]]] =
      allSuccess((obj \ "topics").extractOpt[JArray].map(_.arr).getOrElse(Nil).map {
        case JNull => Right(Nil)
        case jstr: JString => parseTopic(jstr).map(Seq(_))
        case jarr: JArray => parseNestedTopics(jarr)
        case other => Left(InvalidParams(msg = s"Unable to parse topics, expected byte data or array but got: $other"))
      })

    def optionalBlockParam(field: String) =
      (obj \ field).extractOpt[JValue].flatMap {
        case JNothing => None
        case other => Some(extractBlockParam(other))
      }

    for {
      fromBlock <- toEitherOpt(optionalBlockParam("fromBlock"))
      toBlock <- toEitherOpt(optionalBlockParam("toBlock"))
      address <- toEitherOpt((obj \ "address").extractOpt[String].map(extractAddress))
      topics <- topicsEither
    } yield Filter(
      fromBlock = fromBlock,
      toBlock = toBlock,
      address = address,
      topics = topics)
  }

  implicit val eth_sign = new JsonDecoder[SignRequest] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, SignRequest] =
      params match {
        case Some(JArray(JString(addr) :: JString(message) :: _)) =>
          for {
            message <- extractBytes(message)
            address <- extractAddress(addr)
          } yield SignRequest(message, address, None)
        case _ =>
          Left(InvalidParams())
      }
  }

  def extractCall(obj: JObject): Either[JsonRpcError, CallTx] = {
    def toEitherOpt[A, B](opt: Option[Either[A, B]]): Either[A, Option[B]] =
      opt.map(_.right.map(Some.apply)).getOrElse(Right(None))

    for {
      from <- toEitherOpt((obj \ "from").extractOpt[String].map(extractBytes))
      to <- toEitherOpt((obj \ "to").extractOpt[String].map(extractBytes))
      gas <- optionalQuantity(obj \ "gas")
      gasPrice <- optionalQuantity(obj \ "gasPrice")
      value <- optionalQuantity(obj \ "value")
      data <- toEitherOpt((obj \ "data").extractOpt[String].map(extractBytes))
    } yield CallTx(
      from = from,
      to = to,
      gas = gas,
      gasPrice = gasPrice.getOrElse(0),
      value = value.getOrElse(0),
      data = data.getOrElse(ByteString("")))
  }

  private def optionalQuantity(input: JValue): Either[JsonRpcError, Option[BigInt]] =
    input match {
      case JNothing => Right(None)
      case o => extractQuantity(o).map(Some(_))
    }

  implicit val daedalus_getAccountTransactions =
    new JsonDecoder[GetAccountTransactionsRequest] with JsonEncoder[GetAccountTransactionsResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetAccountTransactionsRequest] =
      params match {
        case Some(JArray(JString(addrJson) :: fromBlockJson :: toBlockJson :: Nil)) =>
          for {
            addr <- extractAddress(addrJson)
            fromBlock <- extractQuantity(fromBlockJson)
            toBlock <- extractQuantity(toBlockJson)
          } yield GetAccountTransactionsRequest(addr, fromBlock, toBlock)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: GetAccountTransactionsResponse): JValue =
      JObject("transactions" -> JArray(t.transactions.map(Extraction.decompose).toList))
  }
}
