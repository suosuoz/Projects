import React, { Component } from 'react';
import Pusher from 'pusher-js';
import format from 'date-fns/format';
import './App.css';



import Spotify from 'spotify-web-api-js';

const spotifyWebApi = new Spotify();

class App extends Component {
  constructor(){
    super();
    const params = this.getHashParams();
    const urlParams = new URLSearchParams(window.location.search);
    const isUserAuthorized = urlParams.has('authorized') ? true : false;
    this.state ={
      isUserAuthorized,
      musicHistory: [],
      loggedIn: params.access_token ? true : false,
      nowPlaying: {
        name: 'Not Checked',
        image: ''
      }
    }
    if (params.access_token){
      spotifyWebApi.setAccessToken(params.access_token)
    }
  }

  componentDidMount() {
    const { isUserAuthorized } = this.state;

    if (isUserAuthorized) {
      fetch('http://localhost:5000/history')
        .then(res => res.json())
        .then(data => {
          this.setState({
            musicHistory: data,
          });
        })
        .catch(error => console.log(error));

      const pusher = new Pusher('6dd16304a8d9e90f8877', {
        cluster: 'eu',
        encrypted: true,
      });

      const channel = pusher.subscribe('spotify');
      channel.bind('update-history', data => {
        this.setState(prevState => {
          const arr = data.musicHistory
            .map(item => {
              const isPresent = prevState.musicHistory.find(
                e => e.played_at === item.played_at
              );
              if (isPresent === undefined) {
                return item;
              } else {
                return null;
              }
            })
            .filter(Boolean);
          return {
            musicHistory: arr.concat(prevState.musicHistory),
          };
        });
      });
    }
  }


  getHashParams() {
    var hashParams = {};
    var e, r = /([^&;=]+)=?([^&;]*)/g,
        q = window.location.hash.substring(1);
    while ( e = r.exec(q)) {
       hashParams[e[1]] = decodeURIComponent(e[2]);
    }
    return hashParams;
  }


  getNowPlaying(){
    spotifyWebApi.getMyCurrentPlaybackState()
      .then((response) => {
        this.setState({
          nowPlaying: {
            name: response.item.name,
            image: response.item.album.images[0].url,
            artist: response.item.artists[0].name
          }
        })
      })
  }


  render() {

    const { isUserAuthorized, musicHistory } = this.state;
    const connectSpotify = isUserAuthorized ? (
      ''
    ) : (
      <a href="http://localhost:5000/login">Connect your Spotify account</a>
    );

    const TableItem = (item, index) => (
      <tr key={item.played_at}>
        <td>{index + 1}</td>
        <td>{item.track_name}</td>
        <td>{format(item.played_at, 'D MMM YYYY, hh:mma')}</td>
      </tr>
    );

    const RecentlyPlayed = () => (
      <div className="recently-played">
        <h2>Recent Tracks</h2>
        <table className="table">
          <thead>
            <tr>
              <th>#</th>
              <th>Song title</th>
              <th>Time</th>
            </tr>
          </thead>
          <tbody>{musicHistory.map((e, index) => TableItem(e, index))}</tbody>
        </table>
      </div>
    );

    return (
    <div className="App">
      <a href="http://localhost:8888">  
        <button>Login with Spotify</button>
      </a>
    




      <div className="App">
        <header className="header">
          <h1>Spotify Listening History</h1>
          <p>View your music history in realtime with Spotify and Pusher</p>

          {connectSpotify}
          {musicHistory.length !== 0 ? <RecentlyPlayed /> : null}
        </header>
      </div>




      <div> 
        Now Playing:
        <br />
        <strong>"{ this.state.nowPlaying.name }"</strong> by { this.state.nowPlaying.artist}
      </div>
      <div>
        <img src={ this.state.nowPlaying.image } style={{width: 200}} />
      </div>
      <button onClick={() => this.getNowPlaying()}>Check Now Playing</button>
    </div>
  );
}

}
export default App;