import {
  Component,
  computed,
  inject,
  input,
  output,
  TemplateRef,
  viewChild,
} from '@angular/core';
import { Sadrzaj, TokenService, UserService } from '../../core';
import { CommonModule, DatePipe } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ButtonComponent } from '../button/button.component';

@Component({
  selector: 'app-content-card',
  standalone: true,
  imports: [DatePipe, CommonModule, RouterModule, ButtonComponent],
  templateUrl: './content-card.component.html',
  styleUrl: './content-card.component.scss',
})
export class ContentCardComponent {
  public sadrzaj = input.required<Sadrzaj>();
  private lekcija = viewChild('lekcija', { read: TemplateRef });
  private poveznica = viewChild('poveznica', { read: TemplateRef });
  private dokument = viewChild('dokument', { read: TemplateRef });
  private kviz = viewChild('kviz', { read: TemplateRef });
  public token = inject(TokenService);
  public user = inject(UserService);
  public query = output<boolean>();
  public datum = computed(() => {
    let date = new Date(
      parseInt(this.sadrzaj().vrijednost.vrijeme_kreiranja.toString()) * 1000,
    );
    return `${date.toLocaleDateString()} ${date.toLocaleTimeString()}`;
  });

  public outlet(): TemplateRef<unknown> {
    switch (this.sadrzaj()?.tip) {
      case 'lekcija':
        return this.lekcija();
      case 'poveznica':
        return this.poveznica();
      case 'dokument':
        return this.dokument();
      default:
        return this.kviz();
    }
  }
}
