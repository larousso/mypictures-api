
import collection.JavaConverters._
import controllers.{AccountsController, AlbumsController, Assets, CommentsController, PicturesController, SessionController}
import play.api.ApplicationLoader.Context
import play.api._
import play.api.routing.Router
import router.Routes
import com.softwaremill.macwire._
import services.FacebookAuth
import services.account.Accounts
import services.account.Accounts.Role
import services.album.Albums
import services.comment.Comments
import services.picture.{Images, Pictures}
import play.filters.cors.CORSComponents

class AppApplicationLoader extends ApplicationLoader {
  def load(context: Context) = {

    // make sure logging is configured
    //Logger.configure(context.environment)

    (new BuiltInComponentsFromContext(context) with AppComponents).application
  }
}

trait AppComponents extends BuiltInComponents with AppModule {
  lazy val assets: Assets = wire[Assets]
  lazy val prefix: String = "/"
  lazy val router: Router = wire[Routes]
}

trait AppModule extends BuiltInComponents with CORSComponents {

  import actorSystem.dispatcher

  override lazy val httpFilters = Seq(corsFilter)

  lazy val accounts: Accounts = new Accounts()(actorSystem)
  lazy val albums = new Albums(accounts)(actorSystem)
  lazy val images = new Images(configuration.getString("app.images.path").get)
  lazy val pictures = new Pictures(albums, images)(actorSystem)
  lazy val comments = new Comments(pictures)(actorSystem)

  val redirectUrl = configuration.getString("facebook.redirectUrl").get
  val appId = configuration.getString("facebook.appId").get
  val appSecret = configuration.getString("facebook.appSecret").get
  lazy val facebookAuth = new FacebookAuth(redirectUrl, appId, appSecret)(actorSystem)

  private val users = configuration.getConfigList("users").get.asScala.toList
  users.foreach( userConfig => {
    accounts.createOrUpdateAccount(Accounts.Account(userConfig.getString("username").get, userConfig.getString("password").get, userConfig.getString("name").get, userConfig.getString("surname").get, Role.fromString(userConfig.getString("role").get)))
  })

  lazy val sessionsController = wire[SessionController]
  lazy val accountsController = wire[AccountsController]
  lazy val albumsController = wire[AlbumsController]
  lazy val picturesController = wire[PicturesController]
  lazy val commentsController = wire[CommentsController]

}