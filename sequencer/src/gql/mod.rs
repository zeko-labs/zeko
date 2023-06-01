mod context;
mod mutations;
mod queries;
mod types;

use async_graphql::{http, Result};
use async_graphql_poem::GraphQL;
use poem::{get, handler, listener::TcpListener, web, IntoResponse, Route, Server};

use crate::{
    da_layer::DALayer,
    gql::{mutations::Mutation, queries::Query},
    ledger::Ledger,
};

use self::context::Context;

#[handler]
pub async fn graphiql() -> impl IntoResponse {
    web::Html(http::GraphiQLSource::build().endpoint("/graphql").finish())
}

pub async fn run(
    port: u16,
    da_layer_client: DALayer,
    ledger: Ledger,
) -> Result<(), std::io::Error> {
    let schema = async_graphql::Schema::build(Query, Mutation, async_graphql::EmptySubscription)
        .data(Context {
            da_layer_client,
            ledger: ledger.into(),
        })
        .finish();

    let app = Route::new().at("/graphql", get(graphiql).post(GraphQL::new(schema)));

    println!("Playground: http://localhost:{}/graphql", port);

    Server::new(TcpListener::bind(format!("0.0.0.0:{}", port)))
        .run(app)
        .await
}
