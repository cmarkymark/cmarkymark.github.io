import React from 'react';
import { Route } from 'react-router-dom';
import Header from './components/header/Header.js';
import Navigation from './components/navigation/Navigation.js';
import About from './components/about/About.js';
import Landing from './components/landing/Landing.js';
import Markdown from './components/markdown/Markdown.js';
import Fascination from './components/fascination/Fascination.js';
import Research from './components/research/Research.js';
import Teaching from './components/teaching/Teaching.js';
import Footer from './components/footer/Footer.js';
import './App.css';

// Import markdown files
function importAll(r) {
  return r.keys().map(r);
}
const fascinationPaths = importAll(
  require.context('./assets/markdown/fascination', true, /\.md$/)
);
const researchPaths = importAll(
  require.context('./assets/markdown/research', true, /\.md$/)
);
const teachingPaths = importAll(
  require.context('./assets/markdown/teaching', true, /\.md$/)
);

class App extends React.Component {

  constructor() {
    super();
    this.state = {
      fascination: {
        filePaths: fascinationPaths
      },
      research: {
        filePaths: researchPaths
      },
      teaching: {
        filePaths: teachingPaths
      }
    };
  }

  // Once App has mounted, markdown files are loaded to state.
  async componentDidMount() {
    const keys = Object.keys(this.state);
    try {
      let i = 0;
      for (i; i < keys.length; i++) {
        let j = 0;
        let markdowns = [];
        for (j; j<this.state[keys[i]].filePaths.length; j++) {
          let fileObject = {
            title: null,
            text: null
          };
          const response = await fetch(this.state[keys[i]].filePaths[j]);
          fileObject.text = await response.text();
          fileObject.title = fileObject.text.match(/<!---[\w\s\S]+?-->/);
          fileObject.title = fileObject.title[0].substring(
            6, fileObject.title[0].length - 4
          );
          markdowns.push(fileObject);
        }
        this.setState({ [keys[i]]: markdowns });
      }
    } catch(error) {
        console.log(error);
      }
    }

  render() {
    // Create router path to specific markdown pages based on title
    const keys = Object.keys(this.state);
    let allPostRoutes = [];
    if (
      !this.state.fascination.filePaths &&
      !this.state.research.filePaths &&
      !this.state.teaching.filePaths)
    {
      for (let i = 0; i < keys.length; i++) {
        for (let j=0; j<this.state[keys[i]].length; j++) {
          let path = "/" + this.state[keys[i]][j].title.replace(/\s/g , "-");
          allPostRoutes.push(
            <Route
              exact path={path}
              key={this.state[keys[i]][j].title}
              render={(props) => <Markdown {...this.state[keys[i]][j]} />}
            />
          );
        }
      }
    }
    // After all markdowns have been loaded and read, you can check state.
    // console.log(this.state);
    return (
      <div className="App">
        <main>
          <Header/>
          <Navigation {...this.state} />
          <div id="content">
            <Route exact path="/" component={Landing} />
            <Route exact path="/about" component={About} />
            <Route
              exact path='/fascination'
              render={() => <Fascination {...this.state.fascination} />}
            />
            <Route
              exact path='/research'
              render={() => <Research {...this.state.research} />}
            />
            <Route
              exact path='/teaching'
              render={() => <Teaching {...this.state.teaching} />}
            />
            {allPostRoutes}
          </div>
          <Footer/>
        </main>
      </div>
    );
  };
};

export default App;
